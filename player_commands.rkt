#lang racket
(require "presentation.rkt")
(require "cmd_store.rkt")
(require "world_init.rkt")
(require "help_procs.rkt")
(require "place.rkt")
(require "item.rkt")
(provide (all-defined-out))

#|Creates the necessary functions to operate in
the world and then they are given simpler commands
for the player such as "look", "take" etc. Binds
a lot of values to remove repeating procedures.|#



#|A quit function to end the game whenever you want to.
It simply closes the window.|#

(add-cmd! "quit"
          (lambda (this-ui arg)
            (send this-ui notify "Bai bai plz come back again!")
            (send this-ui close-ui)))


#|An inventory function that lets the player see what they've gathered.
Handles empty inventories and uses string-join function to gather them
up in one line with space between.|#


(add-cmd! "inventory"
          (lambda (this-ui arg)
            (let ([inventory (send dolan get-inventory)])
            (cond
              ((null? inventory)
               (send this-ui present "Dolan's pockets are empty."))
              (else
               (send this-ui present
                     (string-join (map (lambda (items)
                            items)
                          inventory))))))))


#|Interact function that lets the player talk with other NPCs
in the world. Handles empty arguments or when the player
tries to talk with someone who isn't there.|#

(add-cmd! "interact"
          (lambda (this-ui arg)
            (let ([person (car arg)]
                  [character-location (send dolan get-place)])
              (cond
                ((null? arg)
                 (send this-ui present "Who do you want Dolan to interact with?"))
                ((false? (occurs? person (send character-location characters)))
                 (send this-ui present "Not possible, try something else."))
                (else
                 (send this-ui present
                       (send (send character-location get-char person) get-talk-line)))))))


#|A look function that lets the player see everything in the
room (unless it's "locked" until specific conditions have been
met). Binds some values to avoid repeating same procedures
and it can also handle possible exits.|#

(add-cmd! "look"
          (lambda (this-ui arg)
            (let* ([character-location (send dolan get-place)]
                  [object (if (null? arg)
                              '()
                              (car arg))]
                  [items-place (send character-location items)]
                  [possible-exits (send character-location exits)])
              (cond
                ((null? arg)
                 (send this-ui present "---")
                 (send this-ui present "Dolan's standing in the: ")
                 (send this-ui present (send character-location get-name))
                 (send this-ui present "Description of what Dolan can see: ")
                 (send this-ui present (send character-location get-description))
                 (send this-ui present "The characters standing in the room: ")
                 (send this-ui present
                       (string-join (map (lambda (char)
                                           char)
                                         (send character-location characters))))
                 (send this-ui present "The items in the room: ")                                                
                 (send this-ui present
                       (string-join (map (lambda (arg-items)
                                           arg-items)
                                         items-place)))
                 (send this-ui present "The possible exits: ")                                                   
                 (send this-ui present
                       (string-join possible-exits)))
                ((occurs? object possible-exits)                                        
                 (send this-ui present "The exit leads to: ")
                 (send this-ui present
                       (send (send character-location get-neighbour! object) get-name)))
                ((not (false? (occurs? object items-place)))             
                 (send this-ui present
                       (send (hash-ref (send character-location items-ht) object) get-description)))
                ((not (false? (send character-location char-there? object)))                   
                 (send this-ui present
                       (send (send character-location get-char object) get-description)))
                (else
                 (send this-ui present "What are you looking at? Type look to see what's in the room."))))))
          

#|A move function that lets the player move around in the world.
The player needs the correct exit to be able to move Dolan. Also
handles no arguments and wrong arguments.|#

(add-cmd! "move"
            (lambda (this-ui exit)
              (let ([character-location (send dolan get-place)]
                    [chosen-exit (car exit)])
              (cond
                ((null? exit)
                 (send this-ui present "Where do you want Dolan to go?"))
                ((occurs? chosen-exit (send character-location exits))
                 (send dolan move-to (send character-location get-neighbour! chosen-exit))
                 (send this-ui present "Dolan went through the exit."))
                (else
                 (send this-ui present "Not a possible exit."))))))


#|A take function that lets the player take certain items in the world
(not all items tho) and which items that can be taken is decided in the
world_init file. Also handles no arguments, how many items can be taken
and wrong arguments. The function will also remove the item from it's place
when taken and update Dolans inventory.|#

(add-cmd! "take"
          (lambda (this-ui arg)
            (let* ([character-location (send dolan get-place)]
                  [object (if (null? arg)
                              '()
                              (car arg))]
                  [object-take (if (null? object)
                                   '()
                                   (send character-location get-item object))])
            (cond
              ((null? arg)
               (send this-ui present "What do you want Dolan to pick up?"))
              ((> (length arg) 1)
               (send this-ui present "Dolan can only pick up one item at a time!"))
              ((false? (occurs? object (send character-location items)))
               (send this-ui present "Not possible, try something else."))
              ((equal? 'Y (send object-take get-take))
               (send dolan receive-item object-take)
               (send character-location remove-item object)
               (send this-ui present "Dolan took the item."))
              (else
               (send this-ui present (send object-take get-error-description)))))))


#|A function that lets the player give items to other NPCs in
the world. The player can only give items to "special-characters"
for it to work. Handles no arguments, too many arguments since the
player can only give one item at a time, wrong arguments and makes
sure to update inventory when item has been given. The special-characters
have certain receive functions that can change the world when the player
gives a "correct" item to them.|#

(add-cmd! "give"
          (lambda (this-ui arg)
            (let* ([character-location (send dolan get-place)]
                   [object (if (null? arg)
                               '()
                               (car arg))]
                   [person (if (< (length arg) 2)
                               '()
                               (cadr arg))])
            (cond
              ((null? arg)
               (send this-ui present "Give what?"))
              ((false? (= (length arg) 2))
               (send this-ui present "What are you doing? Type help if you don't understand the command."))
              ((false? (occurs? object (send dolan get-inventory)))
               (send this-ui present "You don't have that item!"))
              ((false? (occurs? person (send character-location characters)))
               (send this-ui present "Who? Can't find that character here!"))
              (else
               (send dolan give object
                (send character-location get-char person) this-ui))))))


#|A simple command to let the player feel like they're starting on something.|#

(add-cmd! "play-game"
          (lambda (this-ui arg)
            (send this-ui notify "Welcome player! Close this window and type help for possible commands.")
            (send this-ui present "You're Dolan Thrump and you're standing in the Oval Office.
There's an angry mob of Journalists in the Press-Room demanding that you give them The-nuclear-football.
Type help for possible commands at your disposal...")))


#|A function that lets the player know which commands they
have at their disposal to complete the game.|#

(add-cmd! "help"
          (lambda (this-ui arg)
            (send this-ui present
                  "Different commands you can enter:
#look ; If you type look you will look around in the room.
Type look and follow it up with items or characters to look at them.
#move ; If you want to move, type move followed up with known exit in the room.
#inventory ; Shows your inventory.
#interact ; Lets you talk to people in the room by writing interact followed
up with the characters name.
#take ; If you want to pick up something, type take followed with the items name.
#quit ; Quit the game.
#give ; Gives a specified item from your inventory to a another character. You type
give followed up with what item to give followed up with whom to give.")))