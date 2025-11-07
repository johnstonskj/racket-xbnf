#lang racket/base

(require 
  racket/bool
  racket/format
  racket/list
  racket/string)

(provide
  ;; enumeration types
  definition-alignment
  definition-alignment?
  block-comment-style
  block-comment-style?
  ;; major style type
  (except-out (struct-out syntax-style) make-syntax-style syntax-style)
  (rename-out (->syntax-style syntax-style))
  ;; additional style functions
  syntax-style-number-rules?
  syntax-style-align-definitions-after-name?
  syntax-style-align-definitions-on-next-line?)

;; ===================================================================================================
;; Syntax Formatter ❱ Style
;; ===================================================================================================

(struct syntax-style (
  ;; (or/c exact-nonnegative-integer? #f)
  rule-number
  ;; (or/c exact-nonnegative-integer? #f)
  rule-number-width
  ;; (or/c definition-alignment? #f)
  align-definitions
  ;; (or/c exact-nonnegative-integer? #f)
  name-width
  ;; (or/c (list exact-nonnegative-integer? exact-nonnegative-integer? boolean?) #f)
  wrap-lines-at
  wrapped-line-indent
  current-indent
  ;; boolean?
  pad-grouping-symbols
  pad-line-comment
  block-comment-style)
  #:transparent
  #:constructor-name make-syntax-style
  #:guard
  (lambda (rule-number
           rule-number-width
           align-definitions
           name-width
           wrap-lines-at
           wrapped-line-indent
           current-indent
           pad-grouping-symbols
           pad-line-comment
           block-comment-style
           struct-id)
    (cond
      ((not (or (exact-nonnegative-integer? rule-number) (false? rule-number)))
       (error (not-an-error rule-number '(exact-nonnegative-integer? #f) 
                            #:for-field 'rule-number struct-id)))
      ((not (or (exact-nonnegative-integer? rule-number-width) (false? rule-number-width)))
       (error (not-an-error rule-number-width '(exact-nonnegative-integer? #f) 
                            #:for-field 'rule-number-width struct-id)))
      ((not (or (definition-alignment? align-definitions) (false? align-definitions)))
       (error (not-an-error align-definitions '(definition-alignment? #f) 
                            #:for-field 'align-definitions struct-id)))
      ((not (or (exact-nonnegative-integer? name-width) (false? name-width)))
       (error (not-an-error name-width '(exact-nonnegative-integer? #f) 
                            #:for-field 'name-width struct-id)))
       ;; ...
      (else 
        (values rule-number
                rule-number-width
                align-definitions
                name-width
                wrap-lines-at
                wrapped-line-indent
                current-indent
                pad-grouping-symbols
                pad-line-comment
                block-comment-style)))))

(define definition-alignment '(after-name next-line))

(define (definition-alignment? v)
  (and (symbol? v) (member v definition-alignment symbol=?)))

(define block-comment-style '(padded open-close open-indent-close))

(define (block-comment-style? v)
  (and (symbol? v) (member v block-comment-style? symbol=?)))

(define (->syntax-style
          #:number-rules? (number-rules? #f)
          #:align-definitions (align-definitions #f) ;; after-name next-line
          #:wrap-lines-at (wrap-lines-at #f)
          #:wrapped-line-indent (wrapped-line-indent 4)
          #:pad-grouping-symbols (pad-grouping-symbols #f)
          #:pad-line-comment (pad-line-comment #f)
          #:block-comment-style (block-comment-style #f))
  (make-syntax-style
    (if number-rules? 0 #f)
    #f
    align-definitions
    #f ;; assigned by display functions
    wrap-lines-at
    wrapped-line-indent
    0
    pad-grouping-symbols
    pad-line-comment
    block-comment-style))

(define (syntax-style-number-rules? style)
  (not (false? (syntax-style-rule-number style))))

(define (syntax-style-align-definitions-after-name? style)
  (let ((align (syntax-style-align-definitions style)))
    (and (symbol? align) (symbol=? align 'after-name))))

(define (syntax-style-align-definitions-on-next-line? style)
  (let ((align (syntax-style-align-definitions style)))
    (and (symbol=? align) (symbol=? align 'next-line))))
