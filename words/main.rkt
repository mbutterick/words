#lang debug racket/base
(require racket/list
         racket/file
         "words.rkt"
         "length-index.rkt"
         "char-index.rkt")

(define (wordlist #:letters [letters "etaoinshrdluw"]
                  #:mandatory [mandatory #f]
                  #:min [min-length 5]
                  #:max [max-length 10]
                  #:hide-plurals [hide-plurals? #f]
                  #:proper-names [proper-names? #f]
                  #:random [random #t]
                  #:max-words [max-words 10]
                  #:all-caps [all-caps? #f]
                  #:initial-caps [initial-caps? #f])
  (define mandatory-cs (if mandatory (string->list mandatory) null))
  (define letter-cs (append (if letters (string->list letters) null) mandatory-cs))
  (for*/fold ([ws null]
              [count 0]
              #:result (map (cond
                              [all-caps? string-upcase]
                              [initial-caps? string-titlecase]
                              [else values]) ws))
             ([idx (in-list ((if random shuffle values) (range (vector-length usable-words))))]
              [w (in-value (vector-ref usable-words idx))]
              [w-charidx (in-value (vector-ref charidx idx))]
              [w-lengthidx (in-value (vector-ref lengthidx idx))]
              #:break (= count (or max-words +inf.0))
              #:when (and
                      ;; between min and max length
                      ((if (<= min-length max-length) <= >=) min-length w-lengthidx max-length)
                      ;; contains each mandatory, case-insensitive
                      (or (not mandatory)
                          (for/or ([c (in-list (map char-downcase (charidx->chars w-charidx)))])
                            (memv c mandatory-cs)))
                      ;; contains only letters + mandatory, case-insensitive
                      (for/and ([c (in-list (map char-downcase (charidx->chars w-charidx)))])
                        (memv c letter-cs))
                      ;; maybe only proper names
                      (if proper-names? (capitalized? w-charidx) (not (capitalized? w-charidx)))
                      ;; maybe hide plurals
                      (if hide-plurals? (not (regexp-match #rx"s$" w)) #t)))
    (values (cons w ws) (add1 count))))

(module+ test
  (time (wordlist)))