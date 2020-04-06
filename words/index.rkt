#lang debug racket/base
(require racket/file
         racket/fasl
         racket/runtime-path)
(provide (all-defined-out))

(define-runtime-path wordidx-file "compiled/words/words-index.rktd")

(define (word-rec-word val) (vector-ref val 0))
(define (word-rec-charint val) (vector-ref val 1))
(define (word-rec-length val) (vector-ref val 2))
(define (word-rec-plural? val) (vector-ref val 3))


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

(define-runtime-path words-file "data/words.rktd")

(require racket/set racket/match)
(define (make-word-recs)
  (define words (for/set ([word (in-lines (open-input-file words-file))])
                         word))
  (for/vector ([word (in-set words)])
              (vector word
                      (word->charidx word)
                      (string-length word)
                      (match (regexp-match #rx"^(.+)e?s$" word)
                        [(list _ prefix) #:when (set-member? words prefix) #true]
                        [_ #false]))))

(define (regenerate-word-index!)
  (make-parent-directory* wordidx-file)
  (with-output-to-file wordidx-file
    (λ () (s-exp->fasl (make-word-recs) (current-output-port)))
    #:exists 'replace))

(define wordrecs
  (and
   (unless (file-exists? wordidx-file)
     (regenerate-word-index!))
   (with-input-from-file wordidx-file
     (λ () (fasl->s-exp (current-input-port))))))

(define (post-installer home-dir)
  (regenerate-word-index!))