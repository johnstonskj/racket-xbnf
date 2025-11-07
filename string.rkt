
#lang racket/base

(require 
  racket/bool
  racket/format
  racket/list
  racket/string
  "./main.rkt"
  "./private/errors.rkt")

(provide
  ;; comments
  comment->string
  ;; terminal
  builtin-terminal->string
  string-terminal->string
  char-terminal->string
  char-range->string
  char-match->string
  ;; non-terminal
  symbol-name->string
  group->string
  choice->string
  repeated->string
  ;; expression / rule
  expression->string
  expression-list->string
  terminator->string
  rule->string
  ;; grammar
  grammar->string)

;; ===================================================================================================
;; Syntax Formatter
;; ===================================================================================================

(define (format-with-symbols v symbols)
  (cond
    ((false? symbols)
     (format "~a" v))
    ((syntax-symbol-prefix? symbols)
     (format "~a~a" (first (syntax-symbols-symbols symbols)) v))
    ((syntax-symbol-suffix? symbols)
     (format "~a~a" v (first (syntax-symbols-symbols symbols))))
    ((syntax-symbol-surround? symbols)
     (format "~a~a~a" (first (syntax-symbols-symbols symbols)) v 
                      (second (syntax-symbols-symbols symbols))))))

;; ===================================================================================================
;; Syntax Formatter ❱ Comments
;; ===================================================================================================

(define (comment->string comment syntax style)
  (let ((block-symbols (syntax-block-comment-symbols syntax))
        (line-symbols (syntax-line-comment-symbols syntax))
        (special-symbols (syntax-special-comment-symbols syntax))
        (indentation (make-string (comment-start-column comment) #\space))
        (style (comment-preferred-style comment)))
        (format
          "~a~n"
          (format-with-symbols 
            (comment-text comment)
            (cond
              ((and (symbol=? style 'line) line-symbols) 
               line-symbols)
              ((and (symbol=? style 'line) (false? line-symbols) block-symbols) 
               block-symbols)
              ((and (symbol=? style 'block) block-symbols) 
               block-symbols)
              ((and (symbol=? style 'block) (false? block-symbols) line-symbols) 
               line-symbols)
              ((and (symbol=? style 'special) special-symbols) 
               special-symbols)
              ((and (symbol=? style 'special) (false? special-symbols) block-symbols) 
               block-symbols)
              ((and (symbol=? style 'special) (false? special-symbols) (false? block-symbols) line-symbols) 
               line-symbols)
              (else 
               (error "no comment form supported")))))))

;; ===================================================================================================
;; Syntax Formatter ❱ Terminals
;; ===================================================================================================

(define (builtin-terminal->string builtin syntax style)
  (symbol->string builtin))

(define (string-terminal->string str syntax style)
  (let ((quote-char (syntax-string-terminal-quote-char syntax)))
     (let* ((unescaped-quote (regexp (format "[^\\]?~a" quote-char)))
            (escaped-quote (format "\\\\~a" quote-char))
            (safe-str (regexp-replace* unescaped-quote str escaped-quote)))
        (format "~a~a~a" quote-char safe-str quote-char))))

(define (char-terminal->string char syntax style)
  (let ((char-symbols (syntax-char-terminal-symbols syntax)))
       (if (false? char-symbols)
           (string-terminal->string (format "~a" char) syntax style)
           (format-with-symbols (char->integer char) char-symbols))))

(define (char-range->string range syntax style)
  (let ((range-symbol (syntax-char-range-symbol syntax)))
    (cond
      ((false? range-symbol) (error "NYI"))
      (else (format "~a~a~a" (char-range-start range) range-symbol (char-range-end range))))))

(define (char-match->string match syntax)
  (let* ((char-symbols (syntax-char-match-symbols syntax))
         (negated (char-match-negated match))
         (match-list (for/list ((match (char-match-matches match)))
                        (cond
                          ((char-terminal? match)
                           (char-terminal->string match syntax))
                          ((char-range?)
                           (char-range->string match syntax))))))
       (cond
        ((and (false? char-symbols) (false? negated))
          (string-join match-list (syntax-choice-symbol syntax)))
        ((and (false? char-symbols) negated)
          (error "cannot format negated match without symbols"))
        (else
          (format-with-symbols (string-join match-list) char-symbols)))))

;; ===================================================================================================
;; Syntax Formatter ❱ Non-Terminals
;; ===================================================================================================

(define (symbol-name->string name syntax style)
  (format-with-symbols name (syntax-rule-name-symbols syntax)))

(define (group->string group syntax style)
  (format-with-symbols 
     (expression-list->string (group-expressions group) syntax style)
     (syntax-grouping-symbols syntax)))

(define (choice->string choice syntax style)
  (string-join 
     (for/list ((expr (choice-expressions choice)))
            (expression->string expr syntax style))
     (syntax-choice-symbol syntax)))

(define (repeated->string repeated syntax style)
  (let* ((expression (repeated-expression repeated))
         (cardinality (repeated-cardinality repeated))
         (symbols (cond
                    ((symbol=? cardinality 'zero-or-one)
                     (syntax-zero-or-one-symbols syntax))
                    ((symbol=? cardinality 'zero-or-many)
                     (syntax-zero-or-many-symbols syntax))
                    ((symbol=? cardinality 'one-or-many)
                     (syntax-one-or-many-symbols syntax))
                    (else (error (not-expecting-error cardinality 
                                                      'repeat-cardinality
                                                      repeat-cardinality
                                                      #f))))))
        (cond
          ((and (symbol=? cardinality 'zero-or-one) (false? symbols))
           (error "cannot render grammar without support for 0..1 cardinality. expression ~v" 
                  repeated))
          ((and (symbol=? cardinality 'zero-or-many) (false? symbols)
                (syntax-zero-or-one-symbols syntax)
                (syntax-one-or-many-symbols syntax))
           (repeated->string (zero-or-one (one-or-many expression))))
          ((and (symbol=? cardinality 'zero-or-many) (false? symbols))
           (error "cannot render grammar without support for 0..* cardinality. expression ~v" 
                  repeated))
          ((and (symbol=? cardinality 'one-or-many) (false? symbols)
                (syntax-zero-or-many-symbols syntax))
           (group->string (->group expression (zero-or-many expression)) syntax style))
          ((and (symbol=? cardinality 'one-or-many) (false? symbols))
           (error "cannot render grammar without support for 1..* cardinality. expression ~v" 
                  repeated))
          (else (format-with-symbols
                  (cond
                    ((and (syntax-symbol-surround? symbols) (group? expression))
                     (expression-list->string (group-expressions expression) syntax style))
                    ((and (syntax-symbol-surround? symbols) (choice? expression))
                     (expression-list->string (choice-expressions expression) syntax style))
                    (else (expression->string expression syntax style)))
                  symbols)))))

;; ===================================================================================================
;; Syntax Formatter ❱ Expressions & Rules
;; ===================================================================================================

(define (expression->string expr syntax style)
  (cond
  ((string-terminal? expr)  (string-terminal->string expr syntax style))
  ((char-terminal? expr) (char-terminal->string expr syntax style))
  ((char-match? expr) (char-match->string expr syntax style))
  ((builtin-terminal? expr) (builtin-terminal->string expr syntax style))
  ((group? expr) (group->string expr syntax style))
  ((choice? expr) (choice->string expr syntax style))
  ((repeated? expr) (repeated->string expr syntax style))
  ((symbol-name? expr) (symbol-name->string expr syntax style))
  (else (error "unidentified expression type `~s` for ~s" (object-name expr) expr))))

(define (expression-list->string expr-list syntax style)
   (let ((separator (syntax-sequence-symbol syntax)))
    (cond
      ((list? expr-list)
        (string-join
          (for/list ((expr expr-list))
            (expression->string expr syntax style))
          separator))
      (else (expression->string expr-list syntax style)))))

(define (terminator->string syntax style)
  (let ((termination-symbols (syntax-termination-symbols syntax)))
    (string-join
      (for/list ((symbol termination-symbols))
       (cond
         ((or (string? symbol) (char? symbol))
          (format "~a" symbol))
         ((symbol=? symbol 'newline)
          (format "~a" #\newline))
         ((symbol=? symbol 'blank-line)
          (format "~a~a" #\newline #\newline))
         (else (error (not-an-error symbol '(string? char? 'newline 'blank-line) #f)))))
      "")))

(define (rule->string rule syntax style)
  (let ((separator (syntax-sequence-symbol syntax)))
    (format
      "~a~a ~a ~a~a"
      (if (style-number-rules? style)
          (~a #:width (style-rule-number-width style)
              (format "[~a]"  (style-rule-number style)))
          "")
      (let ((name-as-string (symbol-name->string (rule-name rule) syntax style)))
        (if (style-name-width style)
            (~a #:width (style-name-width style) name-as-string)
            name-as-string))
      (syntax-defn-symbol syntax)
      (let ((expressions (rule-expressions rule)))
        (expression-list->string expressions syntax style))
      (terminator->string syntax style))))

;; ===================================================================================================
;; Syntax Formatter ❱ Grammar
;; ===================================================================================================

(define (grammar->string grammar syntax c-style)
  (let ((c-style (struct-copy 
                 style
                 c-style
                 (name-width (if (style-align-definitions-after-name? c-style)
                                 (apply max (for/list ((rule (grammar-rules grammar)))
                                              (string-length (symbol->string (rule-name rule)))))
                                 (style-align-definitions c-style)))
                 (rule-number-width (if (style-number-rules? c-style)
                                        (+ 3 (string-length (number->string (length (grammar-rules grammar)))))
                                        (style-rule-number-width c-style))))))
    (string-join
      (flatten
        (append
          (for/list ((comment (grammar-comments grammar))
                     #:when (symbol=? (comment-placement comment) 'before))                  
             (comment->string comment syntax c-style))
          (for/list ((rule (grammar-rules grammar))
                     (rule-number (in-naturals 1)))
             (rule->string rule syntax (if (style-number-rules? c-style)
                                           (struct-copy style
                                                        c-style
                                                        (rule-number rule-number))
                                           c-style)))
          (for/list ((comment (grammar-comments grammar))
                     #:when (symbol=? (comment-placement comment) 'after))                  
             (comment->string comment syntax c-style))))
      "")))
