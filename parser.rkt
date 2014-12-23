#lang racket

(require "generator.rkt")

(provide make-phrase-ht
         translate-phrases)

;; Interleave lst1 and lst2 with lst1[0] starting first (if lst1 != null).
(define (zip lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else (append (list (car lst1) (car lst2))
                  (zip (cdr lst1) (cdr lst2)))]))

;; Make a list of all the phrase symbols in the line.
(define (line-to-phrases str)
  (define (remove-brackets str)
    (regexp-replace #rx"\\(\\((.+?)\\)\\)" str "\\1"))
  (map string->symbol 
       (map remove-brackets 
            (map string-downcase 
                 (regexp-match* #rx"\\(\\(.+?\\)\\)" str)))))

;; Make a list of all the non-phrase strings in the line.
(define (line-to-texts str)
  (regexp-split #rx"\\(\\(.+?\\)\\)" str))

;; Replace first occurrences of opt-rx in each string in list and add strings
;; with those occurrences removed to the list.
(define (trans-opt-lst opt-rx lst)
  (define (translate-optional opt-rx str)
    (cond
      [(regexp-match? opt-rx str) 
       (cons (regexp-replace opt-rx str "((\\1))") 
             (regexp-replace opt-rx str ""))]
      [else str])) 
  (flatten (map (λ (str) (translate-optional opt-rx str)) lst)))

;; Take a list of strings and recursively translate phrases.
(define (trans-opt-recurs lst)
  (define opt-rx1 #rx"\\(\\(!\\?(.+?)\\)\\)")
  (define opt-rx2 #rx"\\(\\(\\?(.+?)\\)\\)")
  (define (optionals-left? opt-rx lst) 
    (ormap (λ (str) (regexp-match? opt-rx str)) lst))
  (cond [(optionals-left? opt-rx1 lst)
         (trans-opt-recurs (trans-opt-lst opt-rx1 lst))]
        [(optionals-left? opt-rx2 lst)
         (trans-opt-recurs (trans-opt-lst opt-rx2 lst))]
        [else lst]))

;; Get label-(list of phrases/text) pair from phrase string and label.
(define (get-raw-phrase str)
  (define between-tabs (string-split str "\t"))
  (define phrase-str (list-ref between-tabs 3))
  (define sentence-frags (string-split phrase-str "/"))
  (define translated-optionals (trans-opt-recurs sentence-frags))
  (define text-lsts (map line-to-texts translated-optionals))
  (define phrases-lsts (map line-to-phrases translated-optionals))
  (define label (string->symbol 
                  (string-downcase (list-ref between-tabs 1))))
  (cons label (map zip text-lsts phrases-lsts)))

;; Map a value (list of lists of strings or string-only phrases) to a phrase.
(define (value->phrase value)
  (define (translate-list lst)
    (define (lst-o-lsts->phrase list-of-lists)
      (phrase (map sentence list-of-lists)))
    (define (selective-phrase elem)
      (cond 
        [(list? elem) (lst-o-lsts->phrase elem)]
        [else elem]))
    (cond [(andmap string? lst) (sentence lst)]
          [else (sentence (map selective-phrase lst))]))
  (phrase (map translate-list value)))

;; Map the labels in a phrase hash-table to a phrase structure (string list).
(define (translate-phrases ht)
  (define (phrase->strings symb)
    (map (λ (x) (filter string? x)) (hash-ref ht symb)))
  (define (picky-phrase->strings maybe-str)
    (cond 
      [(symbol? maybe-str) (phrase->strings maybe-str)]
      [else maybe-str]))
  (make-immutable-hash
    (hash-map ht 
              (λ (key value) 
                 (cons key 
                       (value->phrase
                         (map (λ (lst) (map picky-phrase->strings lst)) 
                            value)))))))

;; Parses file "filename" and returns all phrases in hash-table phrase-ht.
(define (make-phrase-ht filename)
  (define (make-raw-ht-iter ht in)
    (begin
      (define next-line (read-line in))
      (cond
        [(eof-object? next-line) ht]
        [else 
          (define raw-phrase (get-raw-phrase next-line))
          (make-raw-ht-iter
                (hash-set ht (car raw-phrase) (cdr raw-phrase)) in)])))
    (begin
      (define in (open-input-file filename))
      (define phrase-ht (make-raw-ht-iter #hash() in))
      (close-input-port in)
      (translate-phrases phrase-ht)))
