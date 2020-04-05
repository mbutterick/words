#lang debug racket/base
(require racket/file
         racket/fasl
         racket/vector
         "words.rkt")
(provide (all-defined-out))

(define lengthidx-file "compiled/lengthidx.rktd")

(define (regenerate-length-index!)
  (s-exp->fasl (vector-map string-length usable-words) (open-output-file lengthidx-file #:exists 'replace)))

(define lengthidx (let ()
                  (unless (file-exists? lengthidx-file)
                    (regenerate-length-index!))
                  (fasl->s-exp (open-input-file lengthidx-file))))

