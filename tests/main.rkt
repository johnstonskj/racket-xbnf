#lang racket/base

(module+ test
  (require rackunit)
  
  (let ((default-style (->style))
      (example (->grammar
                  #:comments '(
                   "Example grammar for a BNF-like syntax."
                  )
                  'bnf
                  (->rule 'top (zero-or-many 'definiton))
                  (->rule 'definition 'rule-name "::=" 'rule)
                  (->rule 'rule 'element (zero-or-many '("|" rule)))
                  (->rule 'element (->choice 'token 'rule-name 'group) (optional 'repeat-operator))
                  (->rule 'group "(" 'rule ")")
                  (->rule 'repeat-operator (->choice "?" "*" "+"))
                  (->rule 'token "\"" (one-or-many 'CHAR) "\"")
                  (->rule 'rule-name (one-or-many 'ALPHA) (zero-or-many (->group (->choice 'ALNUM #\- #\_)))))))
   ;; fails (display-grammar example bnf-syntax)
   (display-grammar example wsn-syntax default-style)
   (display-grammar example ebnf-syntax default-style)
   (display-grammar example abnf-syntax default-style)
   (display-grammar example scribble-bnf-syntax default-style)
   (display-grammar example w3c-xml-syntax (->style #:number-rules? #t #:align-definitions 'after-name)))

  (check-equal? (+ 2 2) 4))
