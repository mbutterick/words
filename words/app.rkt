#lang debug racket/gui
(require words)

(define app-mono-fam
  (for*/first ([preferred
                '("Triplicate T4" "Menlo" "Consolas" "Andale Mono" "Courier")]
               [mono-fam (in-list (get-face-list 'mono))]               
               #:when (equal? preferred mono-fam))
              preferred))

(define app-font-size 16)
(define app-font (make-font #:face (send normal-control-font get-face) #:size app-font-size))

(define window (new frame% [label "Words"]
                    [width 700]
                    [height 700]
                    [x 100]
                    [y 100]
                    [alignment '(left top)]
                    [spacing 6]
                    [border 6]))


(define current-optional (make-parameter "etaoinshrdluw"))
(define current-omit (make-parameter #f))
(define current-mandatory (make-parameter #f))
(define current-combo (make-parameter #f))
(define refresh-thread (thread void))
(define (buffered-refresh)
  (set! refresh-thread (let ()
                         (kill-thread refresh-thread)
                         (thread (λ () (sleep 0.2) (refresh-wordbox))))))

(define (update-text-field! tf param)  
  (param (match (send tf get-value)
           [(? non-empty-string? str) str]
           [_ #false]))
  ;; put delay on refresh so that rapid typing
  ;; doesn't trigger too many refreshes
  (buffered-refresh))

(define (make-text-field param str [parent-panel #f])
  (new text-field%	 	 
       [parent (or parent-panel window)]
       [label str]
       [font app-font]
       [horiz-margin (if parent-panel 0 12)]
       [init-value (or (param) "")]
       [min-width 330]
       [stretchable-width #f]
       [callback (λ (tf evt) (update-text-field! tf param))]))

(let ([optional-letter-panel (new horizontal-panel%
                                  [parent window]
                                  [horiz-margin 6]
                                  [alignment '(left top)]
                                  [stretchable-height #false])])
  (define tf-optional
    (make-text-field current-optional "optional letters" optional-letter-panel))
  (define ((tf-optional-button-callback str) button evt)
    (send tf-optional set-value str)
    (update-text-field! tf-optional current-optional))
  (for ([label-str '("clear" "a-z" "etaoinshrdluw")]
        [str '("" "abcdefghijklmnopqrstuvwxyz" "etaoinshrdluw")])
       (new button%
            [label label-str]
            [parent optional-letter-panel]
            [font app-font]
            [callback (tf-optional-button-callback str)])))

(for ([param (list  current-omit current-mandatory current-combo)]
      [str '("omitted letters" "mandatory letters" "mandatory combo")])
     (make-text-field param str))

(define current-min-size (make-parameter 3))
(define current-max-size (make-parameter 20))

(for ([param (list current-min-size current-max-size)]
      [start-size (list (current-min-size) (- (current-max-size) 10))]
      [end-size (list (+ (current-min-size) 10) (current-max-size))]
      [label-str '("shortest " "longest  ")]
      [selected-item (list 0 10)])
     (new radio-box%
          [parent window]
          [label label-str]
          [font app-font]
          [horiz-margin 12]
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
                           [alignment '(left top)]
                           [stretchable-height #false])])
  (for ([param (list current-proper-names-choice current-hide-plurals)]
        [msg '("show proper names" "hide plurals")])
       (new check-box%
            [parent checkbox-panel]
            [label msg]
            [font app-font]
            [horiz-margin 6]
            [callback (λ (cb evt)
                        (param (send cb get-value))
                        (refresh-wordbox))])))

(define current-case-choice (make-parameter #f))
(define rb-casing
  (new radio-box%
       [parent window]
       [label "casing "]
       [font app-font]
       [horiz-margin 12]
       [style '(horizontal)]
       [choices '("default" "Title Case" "lowercase" "UPPERCASE")]
       [callback (λ (rb evt)
                   (current-case-choice (match (send rb get-selection)
                                          [1 'title]
                                          [2 'lower]
                                          [3 'upper]
                                          [_ #false]))
                   (refresh-wordbox))]))

(define (refresh-wordbox)
  (define ed (send wordbox get-editor))
  (send ed erase)
  (define wordlist (make-words #:count (current-word-count)
                               #:letters (current-optional)
                               #:omit (current-omit)
                               #:mandatory (current-mandatory)
                               #:combo (current-combo)
                               #:case (current-case-choice)
                               #:min (current-min-size)
                               #:max (current-max-size)
                               #:proper-names (current-proper-names-choice)
                               #:hide-plurals (current-hide-plurals)))
  (send ed insert
        (string-join
         (match wordlist
           [(list words ..1) words]
           [_ (list "[no matching words]")]) " "))
  (update-copy-button-label (length wordlist)))

(define current-word-count (make-parameter 50))

(let ([button-panel (new horizontal-panel% [parent window]
                         [alignment '(left top)]
                         [stretchable-height #false])])
  (for ([count (in-list '(50 100 200 400 800 1600))])
       (define count-str (format "~a" count))
       (new button% [parent button-panel]
            [label count-str]
            [font app-font]
            [callback (λ (button evt)
                        (current-word-count (string->number count-str))
                        (refresh-wordbox))])))

(define wordbox (new text-field%
                     [label #f]
                     [style '(multiple)]
                     [parent window]
                     [font (make-font #:face app-mono-fam #:size app-font-size)]))

(let ([ed (send wordbox get-editor)])
  (send ed set-line-spacing (* app-font-size 0.4))
  (send ed set-padding 6 3 6 3))

(define (words-to-clipboard item evt)
  (send the-clipboard set-clipboard-string (send wordbox get-value) (send evt get-time-stamp)))

(define button-copy
  (new button% [parent window]
       [label "copy words"]
       [font app-font]
       [stretchable-width #true]
       [callback words-to-clipboard]))


(define menubar (new menu-bar%
                     [parent window]))

(define menu-edit (new menu%
                       [parent menubar]
                       [label "Actions"]))

(define (make-menu-item str char proc)
  (new menu-item%
       [parent menu-edit]
       [label str]
       [shortcut char]
       [callback proc]))

(define menu-item-copy (make-menu-item "Copy" #\C words-to-clipboard))

(define menu-item-refresh
  (make-menu-item "Regenerate" #\R (λ (thing evt) (refresh-wordbox))))

(define (update-copy-button-label count)
  (send button-copy set-label (format "copy ~a words" count)))

(define (change-word-count amt)
  (define new-count (max (+ (current-word-count) amt) 0))
  (current-word-count new-count))

(define menu-item-more-words
  (make-menu-item "More" #\= (λ (thing evt)
                               (change-word-count 25)
                               (refresh-wordbox))))

(define menu-item-fewer-words
  (make-menu-item "Fewer" #\- (λ (thing evt)
                                (change-word-count -25)
                                (refresh-wordbox))))

(refresh-wordbox)
(send window show #t)

