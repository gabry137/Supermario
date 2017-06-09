#lang racket
(require "presentation.rkt")
(require "item.rkt")
(require "character.rkt")
(require "place.rkt")
(require "cmd_store.rkt")
(require "help_procs.rkt")
(provide (all-defined-out))


#|Adds some places, items, characters, special characters to the world and connects everything to its proper place.|#

;Places

(define situation-room
  (new place%
       [name "Situation-Room"]
       [description
        "The situation room for crises. You can see the nuclear football here."]
       [exits-ht (make-hash)]))

(define hallway
  (new place%
       [name "Hallway"]
       [description
        "The hallway between the oval office and situation room."]
       [exits-ht (make-hash)]))

(define oval-office
  (new place%
       [name "Oval-Office"]
       [description
        "The oval office where Dolan makes all his decisions. You can see a red phone on the desk."]
       [exits-ht (make-hash)]))

(define press-room
  (new place%
       [name "Press-Room"]
       [description
        "A lot of angry journalists waiting for the nuclear football."]
       [exits-ht (make-hash)]))


;Items

(define nuclearfootball
  (new item%
       [name "The-nuclear-football"]
       [description
        "The briefcase that can authorize a nuclear attack!"]
       [take 'Y]
       [error-description 'no-need]))

(define redphone
  (new item%
       [name "The-red-phone"]
       [description
        "A red phone. It does nothing."]
       [take 'N]
       [error-description "It's stuck! Dolan can't take it."]))

(define tupe
  (new item%
       [name "Tupe"]
       [description
        "A blonde tupé that looks kinda plastic."]
       [take 'Y]
       [error-description 'no-need]))


;Characters

#|The character the player will move around and engage with.|#

(define dolan 
  (new character%
       [name "Dolan"]
       [talk-line
        "China china china."]
       [description
        "You are Dolan Thrump."]))


;Special characters, trigger procedure is for connecting two rooms when Ivanka gets the item she wants.

(define ivanka
  (new special-character1%
       [name "Ivanka"]
       [talk-line
        "Ivanka: I won't let you enter the Press Room without your tupé! I think I saw it in the hallway so give it to me and I'll put it on."]
       [description "It's your wife Ivanka Thrump."]
       [trigger (lambda () (connect-places! oval-office "Press-door" press-room "Steel-door"))]))

(define journalists
  (new special-character2%
       [name "Journalists"]
       [talk-line
        "Journalists: Give us the nuclear football now or we release the photos!"]
       [description "It's an angry mob of journalists holding some suspicious photos of Dolan and candy."]))


#|Connections between rooms.|#

(connect-places! hallway "Downstairs" situation-room "Upstairs")  
(connect-places! oval-office "Hallway-door" hallway "Wooden-door")


#|Connects the characters to the rooms.|#

(send dolan move-to oval-office)
(send ivanka move-to oval-office)
(send journalists move-to press-room)


#|Connects the items to the rooms.|#

(send nuclearfootball move-to situation-room)
(send redphone move-to oval-office)
(send tupe move-to hallway)

