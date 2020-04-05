#lang debug racket/base
(require racket/file
         racket/fasl
         racket/vector
         "words.rkt")
(provide (all-defined-out))

(define (char->bitindex c)
  ;; 64-bit layout
  ;; __________ZYXWVUTSRQPONMLKJIHGFEDCBA______zyxwvutsrqponmlkjihgfedcba
  (cond
    [(char<=? #\a c #\z) (- (char->integer c) 97)] ; 97 = (char->integer #\a)
    [(char<=? #\A c #\Z) (- (char->integer c) 33)] ; 65 = (char->integer #\A)
    [else 0]))

(define (word->charidx word)
  (apply bitwise-ior
         (for/list ([c (in-string word)])
           (expt 2 (char->bitindex c)))))

(define (bitindex->char i)
 (cond
   [(<= 0 i 26) (integer->char (+ i 97))]
   [(<= 32 i 59) (integer->char (+ i 33))]
   [else (error 'bong)]))

(define (charidx->chars int)
  (for/list ([i (in-range 64)]
             #:when (bitwise-bit-set? int i))
    (bitindex->char i)))

(define charidx-file "compiled/charidx.rktd")

(define (regenerate-char-index!)
  (s-exp->fasl (vector-map word->charidx usable-words) (open-output-file charidx-file #:exists 'replace)))

(define charidx (let ()
                  (unless (file-exists? charidx-file)
                    (regenerate-char-index!))
                  (fasl->s-exp (open-input-file charidx-file))))

(define (contains-char? charidx-entry c)
  (bitwise-bit-set? charidx-entry (char->bitindex c)))

(define capitalized-mask
  (for/sum ([i (in-range 32 59)])
    (expt 2 i)))

(define (capitalized? charidx-entry)
  ;; a cap only appears at the beginning of a word,
  ;; so it's sufficient to test whether a cap exists in the idx
  (positive? (bitwise-and charidx-entry capitalized-mask)))

(module+ test
  (require rackunit racket/vector)
  (check-equal? (vector-length (vector-filter (λ (ce) (contains-char? ce #\z)) charidx)) 7830)
  (check-equal? (charidx->chars (word->charidx "abuzz")) '(#\a #\b #\u #\z)))
