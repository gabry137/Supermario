#lang racket

(provide adventure-UI%)

#|

This code somewhat deliberately left ugly. If you want to refactor it into someting
better, rewrite the code without changing the intended behaviour and interface,
feel free. But don't change the interface.


//Anders MÃ¤rak Leffler
|#

(require racket/gui)

(define adventure-UI%
  (class object%
    (init-field
     [window-name "My game name"]
     [height 500]
     [width 500]
     [place-name "Default place. Change when the player moves!"]
     [handle-input
      (lambda (this-UI user-command user-arguments)
        ;; Handle-input takes two arguments: a string user-command (eg "take")
        ;; and a list of arguments (eg ("key"), ("a" "step" "back") or ()).
        ;; What you do with these is up to you!
        
        
        ;; The default behaviour here is to create a long string.        
        (present
         (string-append
          "User wants to <"
          user-command
          "> with the arguments <"
          (string-join user-arguments)
          ">")))])
    
    
    ;; Append entered string to that shown in the output field.    
    ;; This is your way to communicate information back to the user.
    ;; (send *my-UI* append-message "Hi")    
    (define/public (present entered-line)      
      (let
          ([message-with-line-feed (string-append entered-line "\n")])
        (send this present-no-lf message-with-line-feed)))
    
    
    ;; Append the entered string to the output field without line feeds.    
    (define/public (present-no-lf entered-string)        
      (send output-field set-value
            (string-append
             (send output-field get-value)           
             entered-string))
      (scroll-to-bottom))
    
    ;; Hide the window
    (define/public (close-ui)
      (send frame show #f)
      (set! frame 0))
    
    
    ;; Clear the output field.
    (define/public (clear-output)
      (send output-field set-value ""))
    
    
    ;; Create a popup box containing message. 
    ;; Blocks until the user clicks OK.
    ;; Clearly has potential for improvement.
    (define/public (notify message)
      (define dialog-box 
        (new dialog% 
             [label message]
             [width 200]
             [height 100]
             [enabled #t]
             [style '(close-button)]
             [parent frame]))
      (new text-field%
           [label #f]
           [parent dialog-box]
           [enabled #f]
           [init-value message])
      (new button%
           [parent dialog-box]
           [label "OK"]
           [callback 
            (lambda args
              (send dialog-box show #f))])
      (send dialog-box show #t))
    
    
    ;; Change the place name displayed above the output field.
    (define/public (set-place-name new-name)
      (send placename set-label new-name))
    
    ;; Scroll to bottom
    (define/private (scroll-to-bottom)
      (let
          ([editor (send output-field get-editor)])
        
        (send 
         editor
         scroll-to-position
         (send editor last-position))
        (send output-field refresh)))
    
    
    ;; --------------------- Windowing
    
    ;; Generate frame (what you see as the window).
    (define frame
      (new frame%
           [label window-name]
           [height height]
           [width width]))
    
    (send frame show #t)
    
    (define placename
      (new message%
           [parent frame]
           [label place-name]))
    
    (define output-panel
      (new vertical-panel%
           [parent frame]))
    
    (define input-panel
      (new vertical-panel%
           [parent frame]))
    
    (define output-field
      (new text-field%
           [parent output-panel]
           [min-height height]
           [label ""]
           [style '(multiple)]
           [enabled #t]))
    
    
    (define input-field
      (new text-field%
           [init-value ""]
           [label ">>> "]
           [parent input-panel]
           [callback
            (lambda (this-field event)
              (let
                  ([all-entered-text (send this-field get-value)])                
                
                (when
                    (and
                     (eq? (send event get-event-type)'text-field-enter)
                     (not (empty-string? all-entered-text)))
                  
                  (handle-input
                   this
                   (command-part all-entered-text)
                   (arguments-part all-entered-text))
                  
                  (send this-field set-value ""))))]))
    
    
    ;; Helper functions. Private   
    
    (define/private (empty-string? str)
      (equal? str ""))
    
    ;; Produces the command part of non-empty user input.
    ;; Eg: "take fish and chips" -> "take"
    (define/private (command-part str)
      (car (string-split str)))
    
    ;; Produce the arguments of non-empty user input.
    ;; Eg "take fish and chips" -> ("fish" "and" "chips")
    ;; "jump" -> ()  ;; empty list
    (define/private (arguments-part str)
      (cdr (string-split str)))
    
    (super-new)))