#lang racket/base

(require 
  racket/bool
  racket/format
  racket/list
  racket/string)

(provide
  ctor-error
  types->string
  error-field-prefix
  not-an-error
  not-valid-error
  not-expecting-error
  invalid-in-list-error)

;; ===================================================================================================
;; General helpers ❱ Error messages
;; ===================================================================================================

(define (ctor-error struct-id)
  (if (false? struct-id)
      "."
      (format " while constructing a new ~a." struct-id)))

(define (types->string types #:with-prefix? (with-prefix? #f))
  (let ((type-str (cond
                    ((string? types) types)
                    ((boolean? types) (~v types))
                    ((symbol? types) (~a types))
                    ((list? types)
                     (format "(or/c ~a)"
                             (string-join (for/list ((type types)) (types->string type)))))
                    (else (error (not-an-error 'types 
                                               "(or/c string? symbol? (list symbol?))" 
                                               #f))))))
    (if with-prefix?
        (string-join
          (list
            (cond
              ((member (string-ref type-str 0) '(#\a #\e #\i #\o #\u #\h)) "an")
              ((string-prefix? type-str "(or/c ") "in")
              (else "a"))
            type-str))
        type-str)))

(define (error-field-prefix field-name)
  (if (false? field-name) 
       "" 
      (format "field ~a " field-name)))

(define (not-an-error value type struct-id #:for-field (field-name #f))
  (format "~avalue ~v is not ~a~a" 
          (error-field-prefix field-name) 
          value 
          (types->string type #:with-prefix? #t) 
          (ctor-error struct-id)))

(define (not-valid-error value type struct-id #:for-field (field-name #f))
  (format "~avalue ~v is not a valid ~a~a" 
          (error-field-prefix field-name) 
          value 
          (types->string type) 
          (ctor-error struct-id)))

(define (not-expecting-error value type expecting struct-id #:for-field (field-name #f))
  (format "~avalue ~v is not a valid ~a, was expecting one of ~v~a" 
          (error-field-prefix field-name) 
          value 
          (types->string type) 
          expecting 
          (ctor-error struct-id)))

(define (invalid-in-list-error list-of-values element-type struct-id #:for-field (field-name #f))
  (format "~aat least one value in list ~v is not a valid ~a~a" 
          (error-field-prefix field-name) 
          list-of-values
          (types->string element-type) 
          (ctor-error struct-id)))
