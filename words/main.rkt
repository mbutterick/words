#lang debug racket/base
(require racket/list
         "index.rkt")
(provide make-words)

(define (make-words #:letters [letters-arg #f]
                    #:mandatory [mandatory #f]
                    #:omit [omit #f]
                    #:combo [combo #f]
                    #:min [min-length-arg 5]
                    #:max [max-length-arg 10]
                    #:hide-plurals [hide-plurals? #t]
                    #:proper-names [proper-names? #f]
                    #:count [count 10]
                    #:case [casing #f])
  (define letters (or letters-arg "abcdefghijklmnopqrstuvwxyz"))
  (define mandatory-cs
    (if (or mandatory combo)
        (remove-duplicates
         (for/list ([c (in-string (string-append (or mandatory "") (or combo "")))])
                   (char-downcase c)) char=?) null))
  (define forbidden-cs
    (remove-duplicates
     (for/list ([c (in-string (or omit ""))])
      (char-downcase c)) char=?))
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
  (define caser (case casing
                  [(up upcase upper uppercase) string-upcase]
                  [(title titlecase) string-titlecase]
                  [(down downcase lower lowercase) string-downcase]
                  [else values]))
  (define min-length (or min-length-arg 0))
  (define max-length (or max-length-arg +inf.0))
  (for*/fold ([word-acc null]
              [count-acc 0]
              #:result word-acc)
             ([idx (in-list (shuffle (range (vector-length wordrecs))))]
              [rec (in-value (vector-ref wordrecs idx))]
              [word-charidx (in-value (word-rec-charint rec))]
              [word (in-value (word-rec-word rec))]
              #:break (= count-acc (or count +inf.0))
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
                      ;; word does not contain forbidden characters
                      (for/and ([fc (in-list forbidden-cs)])
                        (not (word-charidx . contains-char? . fc)))
                      ;; maybe only proper names
                      (if proper-names?
                          (capitalized? word-charidx)
                          (not (capitalized? word-charidx)))
                      ;; maybe hide plurals
                      (or (not hide-plurals?)
                          (not (word-rec-plural? rec)))))
    (values (cons (caser word) word-acc) (add1 count-acc))))

(module+ test
  (require rackunit)
  (time (make-words))
  (check-equal? (sort (make-words #:mandatory "xyz" #:combo #false #:letters "etaoinshrdluw") string<?)
                  '("azoxy" "dysoxidize" "isazoxy" "oxytonize" "rhizotaxy" "zootaxy")))