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
  (define mandatory-cs
    (if mandatory (remove-duplicates (map char-downcase (string->list mandatory)) char=?) null))
  (define letter-cs (remove-duplicates (append (if letters (map char-downcase (string->list letters)) null) mandatory-cs) char=?))
  (define letter-cs-charidx (word->charidx (list->string letter-cs)))
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
                      ;; word contains each mandatory char, case-insensitive
                      (or (not mandatory)
                          (for/and ([mc (in-list mandatory-cs)])
                                   (w-charidx . contains-char? . mc)))
                      ;; word contains only letters + mandatory, case-insensitive
                      (for/and ([wc (in-list (map char-downcase (charidx->chars w-charidx)))])
                               (letter-cs-charidx . contains-char? . wc))
                      ;; maybe only proper names
                      (if proper-names? (capitalized? w-charidx) (not (capitalized? w-charidx)))
                      ;; maybe hide plurals
                      (if hide-plurals? (not (regexp-match #rx"s$" w)) #t)))
    (values (cons w ws) (add1 count))))

(module+ test
  (require rackunit)
  (time (wordlist))
  (check-equal? (sort (wordlist #:mandatory "xyz") string<?)
                '("azoxy" "dysoxidize" "isazoxy" "oxytonize" "rhizotaxy" "zootaxy")))