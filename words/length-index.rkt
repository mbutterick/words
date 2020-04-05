#lang debug racket/base
(require racket/file
         racket/fasl
         "words.rkt")
(provide (all-defined-out))

(define lengthidx-file "data/lengthidx.rktd")

(define (regenerate-length-index!)
  (s-exp->fasl (map string-length usable-words) (open-output-file lengthidx-file #:exists 'replace)))

(define lengthidx (let ()
                  (unless (file-exists? lengthidx-file)
                    (regenerate-length-index!))
                  (list->vector (fasl->s-exp (open-input-file lengthidx-file)))))

