#lang racket/base

(require 
  racket/bool
  racket/format
  racket/list
  racket/string
  "./private/errors.rkt")

(provide
  (except-out (struct-out comment) make-comment comment)
  (rename-out (->comment comment))
  ;; terminals
  builtin-terminals
  char-terminal?
  (struct-out char-range)
  (except-out (struct-out char-match) make-char-match char-match)
  (rename-out (->char-match char-match))
  string-terminal?
  builtin-terminal?
  terminal?
  ;; non-terminals
  (except-out (struct-out group) make-group group)
  (rename-out (->group group))
  (except-out (struct-out choice) make-choice choice)
  (rename-out (->choice choice))
  symbol-name?
  non-terminal?
  repeat-cardinality
  repeat-cardinality?
  (except-out (struct-out repeated) make-repeated repeated)
  (rename-out (->repeat repeated))
  zero-or-one
  optional
  zero-or-many
  one-or-many
  ;; expressions
  expression?
  expression-list?
  ;; rules
  (except-out (struct-out rule) make-rule rule)
  (rename-out (->rule rule))
  optional
  zero-or-many
  one-or-many
  ;; grammar
  (except-out (struct-out grammar) make-grammar grammar)
  (rename-out (->grammar grammar)))

;; ===================================================================================================
;; Grammar Model ❱ Comments
;; ===================================================================================================

(define comment-styles '(line block special))

(define (comment-style? v) (member v comment-styles symbol=?))

(define comment-placements '(before after))

(define (comment-placement? v) (member v comment-placements symbol=?))

(struct comment (text preferred-style placement start-column)
  #:transparent
  #:constructor-name make-comment
  #:guard 
  (lambda (text preferred-style placement start-column struct-id)
    (cond
      ((not (string? text)) 
        (error (not-an-error #:for-field 'text text 'string struct-id)))
      ((false? (comment-style? preferred-style))
        (error (not-expecting-error preferred-style 'comment-style comment-styles struct-id)))
      ((false? (comment-placement? placement))
        (error (not-expecting-error placement 'comment-placement comment-placements struct-id)))
      ((not (exact-nonnegative-integer? start-column))
        (error (not-an-error #:for-field 'start-column start-column 'exact-nonnegative-integer struct-id)))
      (else (values text preferred-style placement start-column)))))

(define (->comment text 
                   #:preferred-style (preferred-style 'block) 
                   #:placement (placement 'before) 
                   #:start-column (start-column 0))
  (make-comment text preferred-style placement start-column))

(define (->comment-list comments)
  (for/list ((comment comments))
    (cond
      ((comment? comment) comment)
      ((string? comment) (->comment comment))
      (else (error "don't know how to make a comment from ~v" comment)))))

;; ===================================================================================================
;; Grammar Model ❱ Terminals
;; ===================================================================================================

(define builtin-terminals 
  '(OCTET CHAR 
    CONTROL CR LF CRLF
    SP HTAB WS 
    DIGIT HEXDIGIT 
    UPPER LOWER ALPHA
    ALNUM
    QUOTE DQUOTE HYPHEN))

(define (char-terminal? v)
  (char? v))

(struct char-range (start end)
  #:transparent
  #:guard 
  (lambda (start end struct-id)
    (cond
      ((not (char? start))
       (error (not-an-error #:for-field 'start start 'char struct-id)))
      ((not (char? end))
       (error (not-an-error #:for-field 'end end 'char struct-id)))
      ((not (char<? start end))
       (error "start, ~v, must be less than end, ~v, ~v" start end (ctor-error struct-id)))
      (else (values start end)))))

(struct char-match (matches negated)
  #:transparent
  #:constructor-name make-char-match
  #:guard 
  (lambda (matches negated struct-id)
    (cond
      ((not (for/and ((m matches))
               (or (char-terminal? m) (char-range? m))))
       (error "a provided match was not a char-terminal or char-range in `~v` ~v" 
              matches (ctor-error struct-id)))
      ((not (boolean? negated))
       (error (not-an-error #:for-field 'negated negated 'boolean struct-id)))
      (else (values matches negated)))))

(define (->char-match #:negated (negated #f) match . more)
  (make-char-match (cons match more) negated))

(define (string-terminal? v)
  (string? v))

(define (builtin-terminal? v)
  (and (symbol? v) (list? (member v builtin-terminals))))

(define (terminal? v)
  (or (char-terminal? v) (char-range? v) (char-match? v) (string-terminal? v) (builtin-terminal? v)))

;; ===================================================================================================
;; Grammar Model ❱ Fields
;; ===================================================================================================

;; field-definition ::= "$" symbol-name ":" expression
;; field-reference  ::= expression "?" symbol-name

;; ===================================================================================================
;; Grammar Model ❱ Non-Terminals
;; ===================================================================================================

(define (symbol-name? v)
  (and (symbol? v)
       (not (member v builtin-terminals))
       (let* ((str (symbol->string v))
              (chars (string->list str)))
         (for/and ((c chars))
           (or (char-lower-case? c)
               (char-upper-case? c)
               (char=? c #\_)
               (char=? c #\-)
               (and (char>=? #\0)
                    (char<=? #\9)))))))

;; ===================================================================================================
;; Grammar Model ❱ Non-Terminals ❱ Grouping
;; ===================================================================================================

(struct group (expressions)
  #:transparent
  #:constructor-name make-group
  #:guard (lambda (expressions struct-id)
                 (cond
                  ((not (for/and ((expr expressions)) (expression? expr)))
                   (error (invalid-in-list-error expressions 'expression struct-id)))
                  (else expressions))))

(define (->one-or-group constructor expr . more)
  (cond
    ((and (list? expr) (empty? more)) 
     (apply constructor (list expr)))
    ((and (list? expr) (not (empty? more))) 
     (apply ->one-or-group constructor (list (append expr more))))
    (else
      (let ((expressions (cons expr more)))
        (apply constructor (list expressions))))))

(define (->group expr . more)
  (apply ->one-or-group make-group expr more))

(struct choice (expressions)
  #:transparent
  #:constructor-name make-choice
  #:guard 
  (lambda (expressions struct-id)
    (cond
      ((not (for/and ((expr expressions)) (expression? expr)))
       (error (invalid-in-list-error expressions 'expression struct-id)))
      (else expressions))))

(define (->choice rule . more)
  (apply ->one-or-group make-choice rule more))

(define repeat-cardinality '(zero-or-one zero-or-many one-or-many))

(define (repeat-cardinality? v)
  (member v repeat-cardinality symbol=?))

(struct repeated (expression cardinality)
  #:transparent
  #:constructor-name make-repeated
  #:guard 
  (lambda (expression cardinality struct-id)
    (cond 
      ((not (expression? expression))
       (error (not-an-error expression 'expression struct-id)))
      ((not (repeat-cardinality? cardinality))
       (error (not-an-error cardinality 'repeat-cardinality struct-id)))
      (else (values expression cardinality)))))

(define (->repeat v cardinality)
  (cond
    ((repeated? v) v)
    ((list? v) 
     (make-repeated (->group v) cardinality))
    ((or (terminal? v) (non-terminal? v)) 
     (make-repeated v cardinality))
    (else 
     (error (not-an-error v '(repeated? list? termina? non-terminal?) #f)))))

(define (zero-or-one v)
  (->repeat v 'zero-or-one))
  
(define optional zero-or-one) ;; alternate name

(define (zero-or-many v)
  (->repeat v 'zero-or-many))

(define (one-or-many v)
  (->repeat v 'one-or-many))

(define (non-terminal? v)
  (or (symbol-name? v) (group? v) (choice? v) (repeated? v)))

;; ===================================================================================================
;; Grammar Model ❱ Expressions & Rules
;; ===================================================================================================

(define (expression? v)
  (or (terminal? v)
      (non-terminal? v) 
      (expression-list? v)))

(define (expression-list? v)
  (and (list? v) (for/and ((expr v))
                    (expression? expr))))

(struct rule (name expressions comments)
  #:transparent
  #:constructor-name make-rule
  #:guard 
  (lambda (name expressions comments struct-id)
    (let ((comments (->comment-list comments)))
      (cond
        ((not (symbol-name? name))
         (error (not-an-error name 'symbol-name struct-id)))
        ((not (for/and ((expr expressions)) (expression? expr)))
         (error (invalid-in-list-error expressions 'expression struct-id)))
        ((not (for/and ((cmt comments)) (comment? cmt)))
         (error (invalid-in-list-error comments 'comment struct-id)))
        (else (values name expressions comments))))))

(define (->rule #:comments (comments '()) name expr . more)
  (make-rule name (cons expr more) comments))

;; ===================================================================================================
;; Grammar Model ❱ Grammar
;; ===================================================================================================

(define (grammar-complete rules)
  (let ((rule-names (for/list ((rule rules)) (rule-name rule))))
       (flatten
         (for/list ((rule rules))
             (for/list ((expr (rule-expressions rule)) 
                        #:when (and (symbol-name? expr)
                               (not (member expr rule-names))))
                 expr)))))

(struct grammar (name rules comments)
  #:transparent
  #:constructor-name make-grammar
  #:guard 
  (lambda (name rules comments struct-id)
    (let ((comments (->comment-list comments)))
      (cond 
        ((not (symbol-name? name))
          (error (not-an-error name 'symbol-name struct-id)))
        ((not (for/and ((rule rules)) (rule? rule)))
          (error (invalid-in-list-error rules 'rule struct-id)))
         ((not (for/and ((cmt comments)) (comment? cmt)))
          (error (invalid-in-list-error comments 'comment struct-id)))
        (else
          (let ((missing-rules (grammar-complete rules)))
            (if (not (empty? missing-rules))
               (error "expressions reference undefined rules ~v~a" 
                      missing-rules
                      (ctor-error struct-id))
               (values name rules comments))))))))

(define (->grammar #:comments (comments '()) name rule . more)
  (make-grammar name (cons rule more) comments))
