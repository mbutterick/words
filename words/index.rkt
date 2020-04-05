#lang debug racket/base
(require racket/file
         racket/fasl
         racket/runtime-path)
(provide (all-defined-out))

(define-runtime-path wordidx-file "compiled/words/words-index.rktd")

(define (word-rec-word val) (vector-ref val 0))
(define (word-rec-charint val) (vector-ref val 1))
(define (word-rec-length val) (vector-ref val 2))


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

(define (contains-char? charidx-entry c)
  (bitwise-bit-set? charidx-entry (char->bitindex c)))

(define capitalized-mask
  (for/sum ([i (in-range 32 59)])
           (expt 2 i)))

(define (capitalized? charidx-entry)
  ;; a cap only appears at the beginning of a word,
  ;; so it's sufficient to test whether a cap exists in the idx
  (positive? (bitwise-and charidx-entry capitalized-mask)))


(define (make-word-recs)
  (define reverse-string (compose1 list->string reverse string->list))
  (define omit-words (map reverse-string (file->lines "data/omit.rktd")))
  (for/vector ([w (in-lines (open-input-file "data/words.rktd"))]
               #:when (and (not (regexp-match "'" w)) ; no apostrophes
                           (regexp-match #rx"^[A-Za-z]+$" w) ; no accented letters
                           (not (member w omit-words)))) ; no bad words
              (vector w
                      (word->charidx w)
                      (string-length w))))

(define (regenerate-word-index!)
  (make-parent-directory* wordidx-file)
  (s-exp->fasl
   (make-word-recs)
   (open-output-file wordidx-file #:exists 'replace)))

(define wordrecs
  (fasl->s-exp (open-input-file (and
                                 (unless (file-exists? wordidx-file)
                                   (regenerate-word-index!))
                                 wordidx-file))))