#lang debug racket/base
(require racket/list
         racket/file
         "words.rkt"
         "char-index.rkt")

(define (wordlist #:letters [letters "etaoinshrdluw"]
                  #:mandatory [mandatory #f]
                  #:min [min-length 3]
                  #:max [max-length 10]
                  #:hide-plurals [hide-plurals? #f]
                  #:proper-names [proper-names? #f]
                  #:random [random #t]
                  #:max-words [max-words 10]
                  #:all-caps [all-caps? #f]
                  #:initial-caps [initial-caps? #f]
                  #:suffix [suffix ""])
  ;; do local filtering here (i.e., filters that are true per query)
  (define taker #f)
  (let* ([ws (for/list ([w (in-list (if taker (take usable-words taker) usable-words))]
                        [w-charidx (in-list (if taker (take charidx taker) charidx))]
                        [idx (in-naturals)]
                        #:when (and
                                ;; between min and max length
                                (<= min-length (string-length w) max-length)
                                ;; contains each mandatory, case-insensitive
                                ;; slow: string match
                                #;(if mandatory
                                    (for/and ([c (in-string mandatory)])
                                             (regexp-match (regexp (format "(?i:~a)" c)) w))
                                    #t)
                                ;; fast: index math
                                (or (not mandatory)
                                    (for*/or ([mandatory-cs (in-value (string->list mandatory))]
                                              [c (in-list (map char-downcase (charidx->chars w-charidx)))])
                                      (memv c mandatory-cs)))
                                ;; contains only letters + mandatory, case-insensitive
                                ;; slow: string match
                                #;(regexp-match (regexp (format "^(?i:[~a]+)$" (string-append letters (or mandatory "")))) w)
                                ;; fast: index match
                                
                                (for*/and ([letter-cs (in-value (string->list letters))]
                                           [c (in-list (map char-downcase (charidx->chars w-charidx)))])
                                  (memv c letter-cs))
                                ;; maybe only proper names
                                (regexp-match (if proper-names? #rx"^[A-Z]" #rx"^[a-z]") w)
                                ;; maybe hide plurals
                                (if hide-plurals? (not (regexp-match #rx"s$" w)) #t)))
                       w)]
         [ws ((if random shuffle values) ws)]
         [ws (if max-words (take ws (min (length ws) max-words)) ws)]
         [ws (map (cond
                    [all-caps? string-upcase]
                    [initial-caps? string-titlecase]
                    [else values]) ws)]
         [ws (if (positive? (string-length suffix))
                 (map (λ (w) (string-append w "." suffix)) ws)
                 ws)])
    ws))

(module+ test
  (time (wordlist)))