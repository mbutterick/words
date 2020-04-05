#lang debug racket/base
(require racket/file
         racket/string)
(provide usable-words)

(define reverse-string (compose1 list->string reverse string->list))

(define omit-words (map reverse-string (file->lines "data/omit.rktd")))

(define (make-wordlist)
  ;; do global filtering here (i.e., filters that are always true)
  (define ws
    (for/list ([w (in-lines (open-input-file "data/words.rktd"))]
               #:when (and (not (regexp-match "'" w)) ; no apostrophes
                           (regexp-match #rx"^[A-Za-z]+$" w) ; no accented letters
                           (not (member w omit-words)) ; no bad words
                           ))
              w))
  ws)

(define usable-words (make-wordlist))