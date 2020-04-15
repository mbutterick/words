#lang debug racket/gui
(require words)

(define window (new frame% [label "Words"]
                    [width 600]
                    [height 500]
                    [x 100]
                    [y 100]))

(define current-optional (make-parameter "etaoinshrdluw"))
(define current-omit (make-parameter #f))
(define current-mandatory (make-parameter #f))
(define current-combo (make-parameter #f))
(define refresh-thread (thread void))
(define (buffered-refresh)
  (set! refresh-thread (let ()
                         (kill-thread refresh-thread)
                         (thread (λ () (sleep 0.2) (refresh-wordbox))))))
(for ([param (list current-optional current-omit current-mandatory current-combo)]
      [str '("optional letters" "omitted letters" "mandatory letters" "mandatory combo")])
  (new text-field%	 	 
       [parent window]
       [label str]
       [init-value (or (param) "")]
       [stretchable-width #f]
       [callback (λ (tf evt)
                   (param (match (send tf get-value)
                            [(? non-empty-string? str) str]
                            [_ #false]))
                   ;; put delay on refresh so that rapid typing
                   ;; doesn't trigger too many refreshes
                   (buffered-refresh))]))

(define current-min-size (make-parameter 3))
(define current-max-size (make-parameter 20))

(for ([param (list current-min-size current-max-size)]
      [start-size (list (current-min-size) (- (current-max-size) 10))]
      [end-size (list (+ (current-min-size) 10) (current-max-size))]
      [label-str '("min length" "max length")]
      [selected-item (list 0 10)])
  (new radio-box%
       [parent window]
       [label label-str]
       [style '(horizontal)]
       [selection selected-item]
       [choices (map number->string (range start-size (add1 end-size)))]
       [callback (λ (rb evt)
                   (param (string->number (send rb get-item-label (send rb get-selection))))
                   (refresh-wordbox))]))

(define current-proper-names-choice (make-parameter #f))
(define current-hide-plurals (make-parameter #f))
(let ([checkbox-panel (new horizontal-panel%
                           [parent window]
                           [alignment '(center center)]
                           [stretchable-height #false])])
  (for ([param (list current-proper-names-choice current-hide-plurals)]
        [msg '("show proper names" "hide plurals")])
    (new check-box%
         [parent checkbox-panel]
         [label msg]
         [callback (λ (cb evt)
                     (param (send cb get-value))
                     (refresh-wordbox))])))

(define current-case-choice (make-parameter #f))
(new radio-box%
     [parent window]
     [label #f]
     [style '(horizontal)]
     [choices '("default" "Title Case" "lowercase" "UPPERCASE")]
     [callback (λ (rb evt)
                 (current-case-choice (match (send rb get-selection)
                                        [1 'title]
                                        [2 'lower]
                                        [3 'upper]
                                        [_ #false]))
                 (refresh-wordbox))])

(define (refresh-wordbox)
  (define ed (send wordbox get-editor))
  (send ed erase)
  (send ed insert
        (string-join
         (match (make-words #:count (current-word-count)
                            #:letters (current-optional)
                            #:omit (current-omit)
                            #:mandatory (current-mandatory)
                            #:combo (current-combo)
                            #:case (current-case-choice)
                            #:min (current-min-size)
                            #:max (current-max-size)
                            #:proper-names (current-proper-names-choice)
                            #:hide-plurals (current-hide-plurals))
           [(list words ..1) words]
           [_ (list "[no matching words]")]) " ")))

(define current-word-count (make-parameter 50))

(let ([button-panel (new horizontal-panel% [parent window]
                         [alignment '(center center)]
                         [stretchable-height #false])])
  (for ([count (in-list '(50 100 500 1000 all))])
    (define count-str (format "~a" count))
    (new button% [parent button-panel]
         [label count-str]
         [callback (λ (button evt)
                     (current-word-count (string->number count-str))
                     (refresh-wordbox))])))

(define wordbox (new text-field%
                     [label #f]
                     [style '(multiple)]
                     [parent window]
                     [font (make-font #:face "Fira Mono OT" #:size 14)]))

(refresh-wordbox)
(send window show #t)

