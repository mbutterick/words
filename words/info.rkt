#lang info

(define post-install-collection "index.rkt")
(define raco-commands '(("words" (submod words/command raco) "issue words command" #f)))

(define gracket-launcher-names '("Words"))
(define gracket-launcher-libraries '("app.rkt"))