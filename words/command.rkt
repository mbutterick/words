#lang debug racket
(require racket/string
         "main.rkt")

(module+ raco
  (define command-name (with-handlers ([exn:fail? (λ (exn) #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))

(module+ main
  (println "this is words command"))

(define (dispatch command-name)
  (define letters "abcdefghijklmnopqrstuvwxyz")
  (define mandatory #false)
  (define combo #false)
  (define min-size #false)
  (define max-size #false)
  (define max-words #false)
  (define hide-plurals #true)
  (define proper-names #false)
  (command-line
   #:program "words"
   #:argv (current-command-line-arguments)
   #:once-each
   [("-l" "--letters") letters-arg
                       "possible letters (default is a-z)"
                       (set! letters letters-arg)]
   [("-m" "--mandatory") mandatory-arg
                         "mandatory letters"
                         (set! mandatory mandatory-arg)]
   [("-c" "--combo") combo-arg
                     "mandatory combo"
                     (set! combo combo-arg)]
   [("-n" "--number") max-words-arg
                      "max number of results"
                      (set! max-words (string->number max-words-arg))]
   [("--min") min-size-arg
              "minimum word lengths"
              (set! min-size (string->number min-size-arg))]
   [("--max") max-size-arg
              "minimum word lengths"
              (set! max-size (string->number max-size-arg))]
   [("-s" "--show-plurals") 
                      "show plural words"
                      (set! hide-plurals #false)]
   [("-p" "--proper-names") 
                      "show proper names"
                      (set! proper-names #true)])
  (displayln (string-join (make-words #:letters letters
                                      #:mandatory mandatory
                                      #:combo combo
                                      #:max-words max-words
                                      #:min min-size
                                      #:max max-size
                                      #:hide-plurals hide-plurals
                                      #:proper-names proper-names) " ")))


