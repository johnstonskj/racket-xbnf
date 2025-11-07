#lang racket/base

(require 
  "./string.rkt")

(provide
  ;; comments
  display-comment
  ;; terminal
  display-builtin-terminal
  display-string-terminal
  display-char-terminal
  display-char-range
  display-char-match
  ;; non-terminal
  display-symbol-name
  display-group
  display-choice
  display-repeated
  ;; expression / rule
  display-expression
  display-terminator
  display-rule
  ;; grammar
  display-grammar)

;; ===================================================================================================
;; Display ❱ Comments
;; ===================================================================================================

(define (display-comment comment syntax style)
  (display (comment->string comment syntax style)))

;; ===================================================================================================
;; Display ❱ Terminals
;; ===================================================================================================

(define (display-builtin-terminal builtin syntax style)
  (display (builtin-terminal->string builtin syntax style)))

(define (display-string-terminal str syntax style)
  (display (string-terminal->string str syntax style)))

(define (display-char-terminal char syntax style)
  (display (char-terminal->string char syntax style)))

(define (display-char-range range syntax style)
  (display (char-range->string range syntax style)))

(define (display-char-match match syntax style) 
  (display (char-match->string match syntax style)))

;; ===================================================================================================
;; Display ❱ Non-Terminals
;; ===================================================================================================

(define (display-symbol-name name syntax style)
  (display (symbol-name->string name syntax style)))

(define (display-group expr syntax style)
  (display (group->string expr syntax style)))

(define (display-choice expr syntax style)
  (display (choice->string expr syntax style)))

(define (display-repeated expr syntax style)
  (display (repeated->string expr syntax style)))

;; ===================================================================================================
;; Display ❱ Expressions & Rules
;; ===================================================================================================

(define (display-expression expr syntax style)
  (display (expression->string expr syntax style)))

(define (display-terminator syntax)
  (display (terminator->string syntax)))

(define  (display-rule rule syntax style)
  (display (rule->string rule syntax style)))

;; ===================================================================================================
;; Display ❱ Grammar
;; ===================================================================================================

(define (display-grammar grammar syntax style)
  (display (grammar->string grammar syntax style)))
