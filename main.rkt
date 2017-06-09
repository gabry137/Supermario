#lang racket
(require "presentation.rkt")
(require "cmd_store.rkt")
(require "world_init.rkt")
(require "player_commands.rkt")
(require "help_procs.rkt")
(require "character.rkt")
(require "place.rkt")
(require "item.rkt")


#|The games GUI that opens up the moment you compile the file.
It's main purpose is to read given commands and execute them.
By typing play-game you get an introduction text.|#

(define (handle-input_ this-ui command arg)
  (cond
    [(valid-cmd? command)
     ((get-proc command) this-ui arg)]
    [else
     (send this-ui present-no-lf "Invalid command: ")
     (send this-ui present command)]))

(define adventure-GUI
  (new adventure-UI%
       [window-name "Dolan's Adventure"]
       [width 500]
       [height 500]
       [handle-input handle-input_]))









