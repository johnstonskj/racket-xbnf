#lang racket/base

(require 
  racket/bool
  racket/format
  racket/list
  racket/string)

(provide
  ;; constants
  *symbol-kleene-star*
  *symbol-kleene-plus*
  *symbol-regexp-optional*
  *c-style-line-comment*
  *lisp-style-line-comment*
  *bash-style-line-comment*
  *c-style-comments*
  *pascal-style-comments*
  *matching-pair-braces*
  *matching-pair-brackets*
  *matching-pair-parenthesis*
  *matching-pair-ltgt*
  ;; enumeration types
  syntax-symbol-placements
  syntax-symbol-placements?
  ;; major syntax symbols type
  (except-out (struct-out syntax-symbols) make-syntax-symbols syntax-symbols)
  ;; syntax symbol constructors
  prefix-symbol
  suffix-symbol
  surrounding-symbols
  ;; additional syntax symbols functions
  syntax-symbol-prefix?
  syntax-symbol-suffix?
  syntax-symbol-surround?
  ;; enumeration types
  ;; major symbol type
  (except-out (struct-out syntax) make-syntax syntax)
  (rename-out (->syntax syntax))
  ;; additional style functions
  ;; predefined syntax
  bnf-syntax
  wsn-syntax
  ebnf-syntax
  abnf-syntax
  scribble-bnf-syntax
  w3c-xml-syntax)

;; ===================================================================================================
;; Grammar Syntax ❱ Symbols
;; ===================================================================================================

(define *symbol-kleene-star* "*")

(define *symbol-kleene-plus* "+")

(define *symbol-regexp-optional* "?")

(define *c-style-line-comment* (prefix-symbols "//"))

(define *lisp-style-line-comment* (prefix-symbols ";"))

(define *bash-style-line-comment* (prefix-symbols "#"))

(define *c-style-comments* (surrounding-symbols "/*" "*/"))

(define *pascal-style-comments* (surrounding-symbols "(*" "*)"))

(define *matching-pair-braces* (surrounding-symbols "{" "}"))

(define *matching-pair-brackets* (surrounding-symbols "[" "]"))

(define *matching-pair-parenthesis* (surrounding-symbols "(" ")"))

(define *matching-pair-ltgt* (surrounding-symbols "<" ">"))

;; ===================================================================================================
;; Grammar Syntax ❱ Symbols
;; ===================================================================================================

(define syntax-symbol-placements '(prefix suffix surround))

(define (syntax-symbol-placement? v) (member v syntax-symbol-placements symbol=?))

(struct syntax-symbols (placement symbols)
  #:transparent
  #:constructor-name make-syntax-symbols
  #:guard 
  (lambda (placement symbols struct-id)
    (cond
      ((not (syntax-symbol-placement? placement))
       (error (not-expecting-error placement 'syntax-symbol-placement syntax-symbol-placements struct-id)))
      ((and (or (symbol=? placement 'prefix) (symbol=? placement 'suffix))
            (not (or (list? symbols)
                     (= (length symbols) 1)
                     (for/and ((v symbols)) (string? v)))))
       (error (not-an-error symbols "list of 1 strings" struct-id)))
      ((and (symbol=? placement 'surround)
            (not (or (list? symbols)
                     (and (>= (length symbols) 2) (<= (length symbols) 3))
                     (for/and ((v symbols)) (string? v)))))
       (error (not-an-error symbols "list of 2-3 strings" struct-id)))
     (else (values placement symbols)))))

(define (prefix-symbol symbols)
  (make-syntax-symbols 'prefix (list symbols)))

(define (suffix-symbol symbols)
  (make-syntax-symbols 'suffix (list symbols)))

(define (surrounding-symbols lhs rhs)
  (make-syntax-symbols 'surround (list lhs rhs)))

(define (syntax-symbol-prefix? symbols)
  (and (not (false? symbols)) (symbol=? (syntax-symbols-placement symbols) 'prefix)))

(define (syntax-symbol-suffix? symbols)
  (and (not (false? symbols)) (symbol=? (syntax-symbols-placement symbols) 'suffix)))

(define (syntax-symbol-surround? symbols)
  (and (not (false? symbols)) (symbol=? (syntax-symbols-placement symbols) 'surround)))

;; ===================================================================================================
;; Grammar Syntax ❱ Syntax Definition
;; ===================================================================================================

(struct syntax
  (;; string?
   defn-symbol
   ;; (or/c syntax-symbols? #f)
   rule-name-symbols
   ;; char?
   string-terminal-quote-char
   ;; (or/c syntax-symbols? #f)
   char-terminal-symbols
   ;; (or/c syntax-symbols? #f)
   char-match-symbols
   ;; (or/c syntax-symbols? #f)
   char-match-negation-symbol
   ;; (or/c syntax-symbols? #f)
   char-range-symbol
   ;; (or/c syntax-symbols? #f)
   grouping-symbols
   ;; (or/c syntax-symbols? #f)
   zero-or-one-symbols
   ;; (or/c syntax-symbols? #f)
   zero-or-many-symbols
   ;; (or/c syntax-symbols? #f)
   one-or-many-symbols
   ;; (or/c string? #f)
   choice-symbol
   ;; (or/c string? #f)
   sequence-symbol
   ;; (or/c string? #f)
   exception-symbol
   ;; (or/c (list-of (or/c string? char? 'newline 'blankline)) #f)
   termination-symbols
   ;; (or/c syntax-symbols? #f)
   line-comment-symbols
   ;; (or/c syntax-symbols? #f)
   block-comment-symbols
   ;; (or/c syntax-symbols? #f)
   special-comment-symbols
   ;; (or/c string? #f)
   reference)
  #:transparent
  #:constructor-name make-syntax)

(define (->syntax 
          #:defn-symbol (defn-symbol "::=")
          #:rule-name-symbols (rule-name-symbols #f)
          #:string-terminal-quote-char (string-terminal-quote-char #\")
          #:char-terminal-symbols (char-terminal-symbols #f)
          #:char-match-symbols (char-match-symbols #f)
          #:char-match-negation-symbol (char-match-negation-symbol #f)
          #:char-range-symbol (char-range-symbol #f)
          #:grouping-symbols (grouping-symbols *matching-pair-parenthesis*)
          #:zero-or-one-symbols (zero-or-one-symbols (suffix-symbol *symbol-regexp-optional*))
          #:zero-or-many-symbols (zero-or-many-symbols (suffix-symbol *symbol-kleene-star*))
          #:one-or-many-symbols (one-or-many-symbols (suffix-symbol *symbol-kleene-plus*))
          #:choice-symbol (choice-symbol " | ")
          #:sequence-symbol (sequence-symbol " ")
          #:exception-symbol (exception-symbol #f)
          #:termination-symbols (termination-symbols '(blank-line))
          #:line-comment-symbols (line-comment-symbols *lisp-style-line-comment*)
          #:block-comment-symbols (block-comment-symbols #f)
          #:special-comment-symbols (special-comment-symbols #f)
          #:reference (reference #f))
  (make-syntax
   defn-symbol
   rule-name-symbols
   string-terminal-quote-char
   char-terminal-symbols
   char-match-symbols
   char-match-negation-symbol
   char-range-symbol
   grouping-symbols
   zero-or-one-symbols
   zero-or-many-symbols
   one-or-many-symbols
   choice-symbol
   sequence-symbol
   exception-symbol
   termination-symbols
   line-comment-symbols
   block-comment-symbols
   special-comment-symbols
   reference))

;; ===================================================================================================
;; Grammar Syntax ❱ Builtin Syntax Definitions
;; ===================================================================================================

(define bnf-syntax
  (->syntax 
    #:rule-name-symbols *matching-pair-ltgt*
    #:zero-or-one-symbols #f
    #:zero-or-many-symbols #f
    #:one-or-many-symbols #f
    #:reference "https://en.wikipedia.org/wiki/Backus–Naur_form"))

(define wsn-syntax
  (->syntax 
    #:defn-symbol "="
    #:zero-or-one-symbols *matching-pair-brackets*
    #:zero-or-many-symbols *matching-pair-braces*
    #:one-or-many-symbols #f
    #:termination-symbols '(#\space #\. blank-line)
    #:line-comment-symbols #f
    #:block-comment-symbols *pascal-style-comments*
    #:reference "https://en.wikipedia.org/wiki/Wirth_syntax_notation"))

(define ebnf-syntax
  (->syntax 
    #:defn-symbol "="
    #:zero-or-one-symbols *matching-pair-brackets*
    #:zero-or-many-symbols *matching-pair-braces*
    #:one-or-many-symbols #f
    #:sequence-symbol ", "
    #:exception-symbol "-"
    #:termination-symbols '(#\space #\; blank-line)
    #:block-comment-symbols *pascal-style-comments*
    #:special-comment-symbols (surrounding-symbols "?" "?")
    #:reference "https://en.wikipedia.org/wiki/Extended_Backus–Naur_form"))

(define abnf-syntax
  (->syntax 
    #:defn-symbol "="
    #:rule-name-symbols *matching-pair-ltgt*
    #:char-terminal-symbols (prefix-symbol "%x")
    #:zero-or-one-symbols *matching-pair-brackets*
    #:zero-or-many-symbols (prefix-symbol "0*")
    #:one-or-many-symbols (prefix-symbol "1*")
    #:choice-symbol " / "
    #:termination-symbols '(blank-line)
    #:reference "https://en.wikipedia.org/wiki/Augmented_Backus–Naur_form"))

(define scribble-bnf-syntax
  (->syntax 
    #:defn-symbol "⩴"
    #:rule-name-symbols (surrounding-symbols "‹" "›")
    #:zero-or-one-symbols *matching-pair-brackets*
    #:zero-or-many-symbols (suffix-symbol *symbol-kleene-star*)
    #:one-or-many-symbols (suffix-symbol *symbol-kleene-plus*)
    #:sequence-symbol " "
    #:termination-symbols '(#\space #\; blank-line)))

(define w3c-xml-syntax
  (->syntax 
    #:defn-symbol "::="
    #:char-terminal-symbols (prefix-symbol "#x")
    #:char-match-symbols *matching-pair-brackets*
    #:char-match-negation-symbol (prefix-symbol "^")
    #:char-range-symbol "-"
    #:exception-symbol "-"
    #:line-comment-symbols #f
    #:block-comment-symbols *c-style-comments*
    #:special-comment-symbols *matching-pair-brackets*
    #:reference "https://www.w3.org/TR/xml/#sec-notation"))
