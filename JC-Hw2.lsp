; ################ CONSTANTS ################

; Definition of each feature to make a card
; E.g -> (index. (string cost attack health))

; For Minions cards
(setq minion_names '((0 . (dragon 3 5 10)) (1 . (lion 1 3 5)) (2 . (eagle 2 2 7))))
(setq minion_colors '((0 . (white 1 0.5 1)) (1 . (silver 1 1 2)) (2 . (gold 1 2 5))))
(setq minion_features '((0 . (3heads 1 2 1)) (1 . (mighty 1 1 2))))

; For Monsters cards
(setq monster_names '((0 . (trex 3 5 10)) (1 . (warrior 1 3 5)) (2 . (wolf 2 2 7))))
(setq monster_colors '((0 . (grey 1 0.5 1)) (1 . (red 1 1 2)) (2 . (dark 1 2 5))))
(setq monster_features '((0 . (mutant 1 2 1)) (1 . (evil 1 1 2))))

; For Spell cards
(setq spell_cards '((0 . (small_healer 2 0 0.2)) (1 . (big_healer 4 0 0.5)) (2 . (double_attack 4 0.18 0)) (3 . (extra_attack 3 0.1 0))))

; ################ FUNCTIONS ################

(defun set-card (lst spellp)
	(setq new_card ())
	(setq new_card (cons (car(cdr(cdr(cdr(cdr(car lst)))))) new_card)) ;Health
	(setq new_card (cons (car(cdr(cdr(cdr(car lst))))) new_card)) ;Attack
	(setq new_card (cons (car(cdr(car lst))) new_card)) ;Name
	(setq new_card (cons spellp new_card)) ;Spell/Monster
	(setq new_card (cons (car (cdr (cdr (car lst)))) new_card)) ;Cost
	)

(defun get-element (lst value spellp)
		(if (eq value (car (car lst)))
			(set-card lst spellp)
			(get-element (cdr lst) value spellp)
			)
	)
(defun mount-creature (a b c)

	)
(defun generate-creature (is_minion x)
	(setq color (random 3))
	(setq feature (random 2))
	(if (< x 20)
		(setq name 0)
		(if (< x 48)
			(setq name 1)
			(setq name 2)
			)
		)
	; True case: Monster
	; False case: Minion
	(if (null is_minion)
		(setq desired_color (get-element monster_colors color nil))
		(setq desired_color (get-element minion_colors color nil))
		)
	(if (null is_minion)
		(setq desired_feature (get-element monster_features feature nil))
		(setq desired_feature (get-element minion_features feature nil))
		)
	(if (null is_minion)
		(setq desired_name (get-element monster_names name nil))
		(setq desired_name (get-element minion_names name nil))
		)
	(setq card ())
	(setq card (cons (+ (car(cdr(cdr(cdr(cdr feature))))) (car(cdr(cdr(cdr(cdr color))))) (car(cdr(cdr(cdr(cdr name))))) )))
	(setq card (cons (+ (car(cdr(cdr(cdr feature)))) (car(cdr(cdr(cdr color)))) (car(cdr(cdr(cdr name)))) )))
	(setq card (concatenate  'string (car(cdr(cdr feature))) (car(cdr(cdr color))) (car(cdr(cdr name))) ))
	(setq card (cons (+ (car(cdr feature)) (car(cdr color)) (car(cdr name)) )))
	)

(generate-creature t 45)
; (defun create-card (is_player)
; 	(setq value (random 100))
; 	; True case: Creature
; 	; False case: Spell
; 	(if (< value 80)
; 		(generate-creature is_player value)
; 		(get-element spell_cards (random 4) t)
; 		)
; 	)

; ################ INITIAL DEFINITIONS ################
; Ask name
(print "Name:")
(setq player_name (read-line))
(print player_name)

(setq player_health 30)
(setq AI_health 30)
; (setq is_player t)
; (setq Hand ((create-card is_player) (create-card is_player) (create-card is_player) (create-card is_player)))
; (setq is_player nil)
; (setq AIHand ((create-card is_player) (create-card is_player) (create-card is_player) (create-card is_player)))

; ################ GAME LOOP (MAIN) ################
; set turn to 1
(setq turn 1)
; repeat forever
(setq game_state t)
(loop
;    set player energy to equal turn number
(setq player_energy turn)
;    print game state
(print "Your Turn")
(print "*INSTRUCTIONS*")
(print "If the monster is already on the field:")
(print "Type the number of the card that you want to play followed by the number of the card that you want to attack")
(print "(Remember 0 is to attack your opponent)")
(print "If the monster or the spell card is on your hand:")
(print "Just type the number of the card, and it will be put in the field.")
;    while player is not finished do  ; PLAYER TURN (read-eval-print loop)
(loop
;       read player command
(setq command (read-line))
;       evaluate player command

;       if Player is dead, print message and exit
(if (<= player_health 0)
	(lambda ()
	(print "LOSER")
	(setq game_state nil)
	)
	)
;       if AI is dead, print message and exit
(if (<= AI_health 0)
	(lambda ()
	(print "WINNER")
	(setq game_state nil)
	)
	)

	(when (<= player_energy 0) (return player_energy))
)
;       print game state
(print "Computer's Turn")
;    set AI energy to equal turn number
(setq AI_energy turn)
;    while AI is not finished to	    ; AI TURN
(loop
;       select AI command

;       evaluate AI command

;       if Player is dead, print message and exit
(if (<= player_health 0)
	(lambda ()
	(print "LOSER")
	(setq game_state nil)
	)
	)
;       if AI is dead, print message and exit
(if (<= AI_health 0)
	(lambda ()
	(print "WINNER")
	(setq game_state nil)
	)
	)
	(when (<= AI_energy 0) (return AI_energy))
)
;       print game state
(print "Let's go to the next turn")
;    increment turn
(setq turn (+ turn 1))
(when (null game_state) (return 0))
)