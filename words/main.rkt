#lang debug racket/base
(require racket/list
         "index.rkt")

(define (make-words #:letters [letters "etaoinshrdluw"]
                    #:mandatory [mandatory #f]
                    #:combo [combo #f]
                    #:min [min-length 5]
                    #:max [max-length 10]
                    #:hide-plurals [hide-plurals? #t]
                    #:proper-names [proper-names? #f]
                    #:max-words [max-words 10]
                    #:all-caps [all-caps? #f]
                    #:initial-caps [initial-caps? #f])
  (define mandatory-cs
    (if (or mandatory combo)
        (remove-duplicates
         (for/list ([c (in-string (string-append (or mandatory "") (or combo "")))])
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
  (define caser (cond
                  [all-caps? string-upcase]
                  [initial-caps? string-titlecase]
                  [else values]))
  (for*/fold ([word-acc null]
              [count 0]
              #:result word-acc)
             ([idx (in-list (shuffle (range (vector-length wordrecs))))]
              [rec (in-value (vector-ref wordrecs idx))]
              [word-charidx (in-value (word-rec-charint rec))]
              [word (in-value (word-rec-word rec))]
              #:break (= count (or max-words +inf.0))
              #:when (and
                      ;; between min and max length
                      ((if (<= min-length max-length) <= >=) min-length (word-rec-length rec) max-length)
                      ;; word contains each mandatory char, case-insensitive
                      (for/and ([mc (in-list mandatory-cs)])
                               (word-charidx . contains-char? . mc))
                      ;; word contains only letters + mandatory, case-insensitive
                      (for/and ([wc (in-list (map char-downcase (charidx->chars word-charidx)))])
                               (letter-cs-charidx . contains-char? . wc))
                      (or (not combo)
                          (regexp-match combo word))
                      ;; maybe only proper names
                      (if proper-names?
                          (capitalized? word-charidx)
                          (not (capitalized? word-charidx)))
                      ;; maybe hide plurals
                      (or (not hide-plurals?)
                          (not (word-rec-plural? rec)))))
    (values (cons (caser word) word-acc) (add1 count))))

(module+ test
  (require rackunit)
  (time (make-words))
  (check-equal? (sort (make-words #:mandatory "xyz" #:combo #false) string<?)
                  '("azoxy" "dysoxidize" "isazoxy" "oxytonize" "rhizotaxy" "zootaxy")))