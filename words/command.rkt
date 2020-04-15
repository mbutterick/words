#lang debug racket
(require racket/string
         "main.rkt"
         "kernscore.rkt")

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
  (define min-size 5)
  (define max-size 10)
  (define count 20)
  (define hide-plurals #true)
  (define proper-names #false)
  (define title-case #false)
  (define uppercase #false)
  (define sort-style #false)
  (define omit #false)
  (define reversed #false)
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
  [("-o" "--omit") omit-arg
                         "forbidden letters"
                         (set! omit omit-arg)]
   
   [("-c" "--combo") combo-arg
                     "mandatory combo"
                     (set! combo combo-arg)]
   [("-n" "--number") count-arg
                      "max number of results"
                      (set! count (string->number count-arg))]
   [("--min") min-size-arg
              "minimum word length"
              (set! min-size (string->number min-size-arg))]
   [("--max") max-size-arg
              "maximum word length"
              (set! max-size (string->number max-size-arg))]
   [("-s" "--show-plurals") 
    "show plural words"
    (set! hide-plurals #false)]
   [("-p" "--proper-names") 
    "show proper names"
    (set! proper-names #true)]
   [("--sort") sort-arg
               "sort order"
               (set! sort-style sort-arg)]
   [("-r" "--reverse") 
               "reverse order"
               (set! reversed #true)]
   #:once-any
   [("-t" "--title-case") 
    "capitalize first letter"
    (set! title-case #true)]
   [("-u" "--uppercase") 
    "capitalize all letters"
    (set! uppercase #true)])
  (define words (make-words #:letters letters
                            #:mandatory mandatory
                            #:omit omit
                            #:combo combo
                            #:count count
                            #:min min-size
                            #:max max-size
                            #:hide-plurals hide-plurals
                            #:proper-names proper-names
                            #:case (cond
                                     [uppercase 'upper]
                                     [title-case 'title]
                                     [else #false])))
  (define-values (sort-key sort-cmp)
    (match sort-style
      ["kernscore" (values kernscore <)]
      ["alpha" (values values string<=?)]
      [_ (values #false #false)]))
  (define sorted-words (cond
                         [sort-key
                          (sort words sort-cmp #:key sort-key #:cache-keys? #true)]
                         [else words]))
  (displayln (string-join ((if reversed reverse values) sorted-words) "\n")))


