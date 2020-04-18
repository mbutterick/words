#lang br
(require pict pict/convert racket/draw icns file/ico)

(define (make-icon-pict size)
  (unsafe-dc (λ (dc dx dy)
               (send dc set-text-foreground "slategray")
               (send dc set-text-background "darkslategray")
               (send dc set-font (make-font #:size size
                                            #:face "Equity Text A"))
               (send dc draw-text "W" (/ size 40) (- (/ size 10))))
             size size))

(send (pict->bitmap (make-icon-pict 96)) save-file "app.png" 'png)

(with-output-to-file "app.icns"
  (λ () (void (write-bytes (pict->icns-bytes (make-icon-pict 1024)))))
  #:exists 'replace)

(write-icos (for/list ([size '(16 32 48 256)])
              (argb->ico size size (pict->argb-pixels (make-icon-pict size)))) "app.ico"
                                                                               #:exists 'replace)