#lang racket/gui
(require words)

(define frame (new frame% [label "Words"]
                   [width 500]
                   [height 500]
                   [x 100]
                   [y 100]))

(define (fill-wordbox [word-count 100])
  (define ed (send wordbox get-editor))
  (send ed erase)
  (send ed insert (string-join (make-words #:count word-count) " " #:after-last " ")))

(define ((make-wordbox-callback word-count) [button #f] [event #f])
  (fill-wordbox word-count))

(let ([button-panel (new horizontal-panel% [parent frame]
                         [alignment '(center center)]
                         [stretchable-height #false])])
  (for ([count '(100 250 500 1000 all)])
       (define count-str (format "~a" count))
       (new button% [parent button-panel]
            [label count-str]
            ; Callback procedure for a button click:
            [callback (make-wordbox-callback (string->number count-str))])))

(define wordbox (new text-field%
                     [label #f]
                     [style '(multiple)]
                     [parent frame]
                     [font (make-font #:face "Fira Mono OT" #:size 14)]))

(send frame show #t)

