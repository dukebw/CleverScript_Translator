#lang racket

(provide sentence
         phrase
         find-combos
         print-language-model)

(require racket/generic)

;; For finding all different possible strings from different inputs.
(define-generics combinable
  (find-combos combinable)
  #:defaults 
  ([string?
     (define (find-combos str) (list str))]))

;; Define sentence -- it is an ordered list of text and phrases.
;; A sentence is whatever exists between two '/' separation characters in the
;; CleverScript file.
(struct sentence (lst)
        #:methods gen:combinable
        [(define (find-combos s) (stringify (sentence-lst s)))])

;; Phrases contain a symbol (label) that corresponds to a sentence in 
;; phrase-ht.
(struct phrase (lst) 
        #:methods gen:combinable 
        [(define/generic super-combos find-combos)
         (define (find-combos p) 
           (define (all-strings? phr) 
             (andmap (λ (x) (string? x)) (phrase-lst phr)))
           (cond 
             [(all-strings? p) (flatten (phrase-lst p))]
             [else (flatten (map super-combos (phrase-lst p)))]))])

;; Sentence-lst input only. This function turns a list of phrases, sentences
;; and strings into an equivalent list of strings.
(define (stringify lst)
  (define (combine str-accum lst)
    (cond
      [(empty? lst) empty]
      [else (cons (string-append str-accum (car lst))
                  (combine str-accum (cdr lst)))]))
  (define (combine-lst lst1 lst2)
    (cond
      [(empty? lst1) empty]
      [else (append (combine (car lst1) lst2)
                    (combine-lst (cdr lst1) lst2))]))
  (cond
    [(empty? lst) empty]
    [(equal? (length lst) 1) (find-combos (car lst))]
    [(string? (car lst)) (combine 
                           (car lst) 
                           (find-combos (sentence (cdr lst))))]
    [else (combine-lst 
            (find-combos (car lst)) 
            (find-combos (sentence (cdr lst))))]))

;; Output a list of strings to file.
(define (print-language-model lst filename)
  (define (lst-to-string lst)
    (string-join lst "\n"
                 #:after-last "\n"))
  (call-with-output-file filename
                         #:exists 'truncate
                         (λ (out) (display (lst-to-string lst) out))))
