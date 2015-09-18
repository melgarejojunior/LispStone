; ################ CONSTANTS ################

; Definition of each feature to make a card
; E.g -> (index. (string cost attack health))

; For Minions cards
(setq minion_names (cons (cons (cons(string "dragon") (cons 3 (cons 5 (cons 10 nil)))) 0)
				   (cons (cons (cons(string "lion") (cons 1 (cons 3 (cons 5 nil)))) 1)
				   (cons (cons (cons(string "eagle") (cons 2 (cons 2 (cons 7 nil)))) 2)
					nil))))

(setq minion_colors (cons (cons (cons(string "white") (cons 1 (cons 0.5 (cons 1 nil)))) 0)
				   (cons (cons (cons(string "silver") (cons 1 (cons 1 (cons 2 nil)))) 1)
				   (cons (cons (cons(string "gold") (cons 1 (cons 2 (cons 5 nil)))) 2)
					nil))))

(setq minion_features (cons (cons (cons(string "3heads") (cons 1 (cons 2 (cons 1 nil)))) 0)
					  (cons (cons (cons(string "mighty") (cons 1 (cons 1 (cons 2 nil)))) 1)
						nil))))

; For Monsters cards
(setq monster_names (cons (cons (cons(string "trex") (cons 3 (cons 5 (cons 10 nil)))) 0)
				   (cons (cons (cons(string "warrior") (cons 1 (cons 3 (cons 5 nil)))) 1)
				   (cons (cons (cons(string "wolf") (cons 2 (cons 2 (cons 7 nil)))) 2)
					nil))))

(setq monster_colors (cons (cons (cons(string "grey") (cons 1 (cons 0.5 (cons 1 nil)))) 0)
				   (cons (cons (cons(string "red") (cons 1 (cons 1 (cons 2 nil)))) 1)
				   (cons (cons (cons(string "dark") (cons 1 (cons 2 (cons 5 nil)))) 2)
					nil))))

(setq monster_features (cons (cons (cons(string "evil") (cons 1 (cons 2 (cons 1 nil)))) 0)
					  (cons (cons (cons(string "mutant") (cons 1 (cons 1 (cons 2 nil)))) 1)
						nil))))

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
(defun get-creature-element (lst i)
	(if (eq (cdr(car lst)) i)
		(setq a (car(car lst)))
		(get-creature-element (cdr lst) i)
		)
	)
(defun generate-creature (is_minion x)
	; Define indexes for color, feature and name list
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
		(setq desired_color (get-creature-element monster_colors color))
		(setq desired_color (get-creature-element minion_colors color))
		)
	(if (null is_minion)
		(setq desired_feature (get-creature-element monster_features feature))
		(setq desired_feature (get-creature-element minion_features feature))
		)
	(if (null is_minion)
		(setq desired_name (get-creature-element monster_names name))
		(setq desired_name (get-creature-element minion_names name))
		)
	(setq new_Mcard ())
	(setq new_Mcard (cons (+ (car (cdr (cdr (cdr desired_name)))) (car (cdr (cdr (cdr desired_color)))) (car (cdr (cdr (cdr desired_feature))))) new_Mcard))
	(setq new_Mcard (cons (+ (car (cdr (cdr desired_name))) (car (cdr (cdr desired_color))) (car (cdr (cdr desired_feature)))) new_Mcard))
	(setq new_Mcard (cons (concatenate 'string (car desired_feature) " " (car desired_color) "-" (car desired_name)) new_Mcard))
	(setq new_Mcard (cons (+ (car (cdr desired_name)) (car (cdr desired_color)) (car (cdr desired_feature))) new_Mcard))
	)

(defun create-card (is_player)
	(setq value (random 100))
	; True case: Creature
	; False case: Spell
	(if (< value 80)
		(generate-creature is_player value)
		(get-element spell_cards (random 4) t)
		)
	)

; ################ INITIAL DEFINITIONS ################
; Ask name
(print "Name:")
(setq player_name (read-line))
(print player_name)

(setq player_health 30)
(setq AI_health 30)
(setq playerp t)
(setq Hand (cons(create-card playerp) (cons (create-card playerp) (cons (create-card playerp) (cons (create-card playerp) nil))))
(setq playerp nil)
(setq AIHand (cons(create-card playerp) (cons (create-card playerp) (cons (create-card playerp) (cons (create-card playerp) nil))))
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