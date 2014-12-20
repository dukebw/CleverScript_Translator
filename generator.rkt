#lang racket

(provide sentence
         phrase)

(require test-engine/racket-tests
         racket/generic)

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
                         (lambda (out)
                           (display (lst-to-string lst) out))))

;; Get a phrase from the phrase table.
(define (get-phrase str) (hash-ref phrase-ht (string->symbol str)))

;; phrase	town		city / town / village / hamlet / suburb / burb 
;; / farming ((town)) / little ((town))					
(define town (phrase (list "city" "town" "village" "hamlet" "suburb" "burb"
                           "farming city" "farming town" "farming village"
                           "farming hamlet" "farming suburb" "farming burb"
                           "little city" "little town" "little village"
                           "little hamlet" "little suburb" "little burb")))

;; phrase	spot		place / spot / location / area / land / part				40	
(define spot (phrase (list "place" "spot" "location" "area" "land" "part")))

;; Create hash-table of phrase, where each entry in the hash-table is a list of
;; sentences with the phrase label as a key.
;; (Manually created with test values for testing purposes.)
(define phrase-ht
  ;; Phrase definitions to reduce verbosity of phrase-ht (test values).
  (hash 
    'guys (phrase (list "men" "guys" "dudes" "boys")) ;; Entry in hash table
    'girls (phrase (list "girls" "ladies" "dames" "broads" "gals" "women"))
    ;; this ((town)) / our ((town)) / my ((town)) / this ((spot)) / my ((spot)) 
    ;; / our ((spot)) / where we are / here
    'currentcity (phrase (list (sentence (list "this " town))
                             (sentence (list "our " town))
                             (sentence (list "my " town))
                             (sentence (list "this " spot))
                             (sentence (list "my " spot))
                             (sentence (list "our " spot))
                             "where we are"
                             "here"))
    'town (phrase (list "city" "town" "village" "hamlet" "suburb" "burb"
                      (sentence (list "farming " town))
                      (sentence (list "little " town))))
    'spot (phrase (list "place" "spot" "location" "area" "land" "part"))))

;; Test sub-module. Run with "raco test generator.rkt"
(module+ test
  (check-expect (find-combos (sentence (list "a" (phrase (list "b" "c")))))
                (list "ab" "ac"))
  (check-expect (find-combos 
                  (sentence (list "abc" (phrase (list "d" "e" "f")) "xyz")))
                (list "abcdxyz" "abcexyz" "abcfxyz"))
  (check-expect (find-combos 
                  (sentence (list "abc" (phrase (list "d" "e" "f")) 
                                  (phrase (list "x" "y" "z")) "ff")))
                (list "abcdxff" "abcdyff" "abcdzff" 
                      "abcexff" "abceyff" "abcezff"
                      "abcfxff" "abcfyff" "abcfzff"))
  (check-expect (find-combos (sentence empty)) empty)
  (check-expect (find-combos 
                  (sentence (list 
                              (phrase (list "c" (phrase (list "d" "e")))))))
                (list "c" "d" "e"))
  (check-expect 
    (find-combos 
      (sentence (list "ab" (phrase (list "c" (phrase (list "d" "e")))))))
                (list "abc" "abd" "abe"))
  ;; ((you're)) in the ((car)).
  ;; you're: You are / you're / hitchBOT ((you're))
  ;; car: car / jeep / van
  (define test-sentence 
    (sentence 
      (list (phrase (list "You are " 
                          "you're " 
                          (sentence 
                            (list "hitchBOT " 
                                  (phrase (list "You are " "you're ")))))) 
            "in the " (phrase (list "car" "jeep" "van")))))
  (check-expect (find-combos test-sentence)
                (list "You are in the car" "You are in the jeep"
                      "You are in the van" "you're in the car" 
                      "you're in the jeep" "you're in the van"
                      "hitchBOT You are in the car" 
                      "hitchBOT You are in the jeep"
                      "hitchBOT You are in the van"
                      "hitchBOT you're in the car" 
                      "hitchBOT you're in the jeep"
                      "hitchBOT you're in the van"))
  (print-language-model (find-combos test-sentence) "test-output")
  (test))
