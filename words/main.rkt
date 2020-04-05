#lang debug racket/base
(require racket/list
         racket/file
         "index.rkt")

(define (make-words #:letters [letters "etaoinshrdluw"]
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
    (if mandatory (remove-duplicates (for/list ([c (in-string mandatory)])
                                               (char-downcase c)) char=?) null))
  (define letter-cs-charidx
    (word->charidx
     (list->string
      (remove-duplicates
       (append (if letters
                   (for/list ([c (in-string letters)])
                             (char-downcase c))
                   null)
               mandatory-cs)
       char=?))))
  
  (define capitalizer (cond
                        [all-caps? string-upcase]
                        [initial-caps? string-titlecase]
                        [else values]))
  
  (for*/fold ([word-acc null]
              [count 0]
              #:result word-acc)
             ([idx (in-list ((if random shuffle values) (range (vector-length wordrecs))))]
              [rec (in-value (vector-ref wordrecs idx))]
              [word (in-value (word-rec-word rec))]
              [word-charidx (in-value (word-rec-charint rec))]
              #:break (= count (or max-words +inf.0))
              #:when (and
                      ;; between min and max length
                      ((if (<= min-length max-length) <= >=) min-length (word-rec-length rec) max-length)
                      ;; word contains each mandatory char, case-insensitive
                      (or (not mandatory)
                          (for/and ([mc (in-list mandatory-cs)])
                                   (word-charidx . contains-char? . mc)))
                      ;; word contains only letters + mandatory, case-insensitive
                      (for/and ([wc (in-list (map char-downcase (charidx->chars word-charidx)))])
                               (letter-cs-charidx . contains-char? . wc))
                      ;; maybe only proper names
                      (if proper-names? (capitalized? word-charidx) (not (capitalized? word-charidx)))
                      ;; maybe hide plurals
                      (if hide-plurals? (not (regexp-match #rx"s$" word)) #t)))
    (values (cons (capitalizer word) word-acc) (add1 count))))

(module+ test
  (require rackunit)
  (time (make-words))
  (check-equal? (sort (make-words #:mandatory "xyz") string<?)
                '("azoxy" "dysoxidize" "isazoxy" "oxytonize" "rhizotaxy" "zootaxy")))