#lang info

(define post-install-collection "index.rkt")
(define raco-commands '(("words" (submod words/command raco) "issue words command" #f)))

#;(define racket-launcher-names '("Words.app"))
#;(define racket-launcher-libraries '("app.rkt"))