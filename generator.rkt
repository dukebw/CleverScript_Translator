#lang racket

;; Define sentence -- it is an ordered list of text and phrases.
;; A sentence is whatever exists between two '/' separation characters in the
;; CleverScript file.
(struct sentence (lst) ;; lst contains only strings, optionals or phrases.
        #:methods gen:custom-write ;; define how sentences are displayed.
        [(define (write-proc sentence port mode)
           (define lst (sentence-lst sentence)) ;; short-form for member list
           (for-each (λ (x) ;; lambda to print each element of sentence-lst
                        (show "~a\n" x port))
                     lst))
         (define (show fmt str port)
           (fprintf port fmt str))])

;; Optionals contain either a string or a phrase.
;; Optional entries appear 0 or 1 times on lines of a given sentence.
(struct optional (entry))

;; Phrases contain a symbol (label) that corresponds to a sentence in phrase-ht.
(struct phrase (label))

;; Phrase definitions to reduce verbosity of phrase-ht.
(define town (phrase 'town)) ;; test values
(define spot (phrase 'spot))

;; Create hash-table of phrase, where each entry in the hash-table is a list of
;; sentences with the phrase label as a key.
;; (Manually created with test values for testing purposes.)
(define phrase-ht 
  (hash 
    'guys (sentence '("men" "guys" "dudes" "boys")) ;; Entry in hash table
    'girls (sentence '("girls" "ladies" "dames" "broads" "gals" "women"))
    ;; this ((town)) / our ((town)) / my ((town)) / this ((spot)) / my ((spot)) 
    ;; / our ((spot)) / where we are / here
    'currentcity (sentence '((sentence '("this" town))
                             (sentence '("our" town))
                             (sentence '("my" town))
                             (sentence '("this" spot))
                             (sentence '("my" spot))
                             (sentence '("our" spot))
                             (sentence "where we are")
                             (sentence "here")))))
