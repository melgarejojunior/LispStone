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
						nil)))

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
						nil)))

; For Spell cards
(setq spell_cards '((0 . ("small_healer" 2 0 0.2)) (1 . ("big_healer" 4 0 0.5)) (2 . ("double_attack" 4 3 0)) (3 . ("extra_attack" 0 4 0))))

; ################ FUNCTIONS ################

;Create a card in the required format
(defun set-card (lst spellp)
	(setq new_card ())
	(setq new_card (cons (car(cdr(cdr(cdr(cdr(car lst)))))) new_card)) ;Health
	(setq new_card (cons (car(cdr(cdr(cdr(car lst))))) new_card)) ;Attack
	(setq new_card (cons (car(cdr(car lst))) new_card)) ;Name
	(setq new_card (cons spellp new_card)) ;Spell/Monster
	(setq new_card (cons (car (cdr (cdr (car lst)))) new_card)) ;Cost
	)
; Search in the list which spell is that value
(defun get-element (lst value spellp)
		(if (eq value (car (car lst)))
			(set-card lst spellp)
			(get-element (cdr lst) value spellp)
			)
	)
; Search in the list which creature is that value
(defun get-creature-element (lst i)
	(if (eq (cdr(car lst)) i)
		(setq a (car(car lst)))
		(get-creature-element (cdr lst) i)
		)
	)
; Create a new creature, mixing NAME, COLOR AND FEATURE
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
	; Create the new creature card
	(setq new_Mcard ())
	(setq new_Mcard (cons (+ (car (cdr (cdr (cdr desired_name)))) (car (cdr (cdr (cdr desired_color)))) (car (cdr (cdr (cdr desired_feature))))) new_Mcard))
	(setq new_Mcard (cons (+ (car (cdr (cdr desired_name))) (car (cdr (cdr desired_color))) (car (cdr (cdr desired_feature)))) new_Mcard))
	(setq new_Mcard (cons (concatenate 'string (car desired_feature) " " (car desired_color) "-" (car desired_name)) new_Mcard))
	(setq new_Mcard (cons (+ (car (cdr desired_name)) (car (cdr desired_color)) (car (cdr desired_feature))) new_Mcard))
	)

; Generalized function to create a card (Spell or Creature)
(defun create-card (is_player)
	(setq value (random 110))
	; True case: Creature
	; False case: Spell
	(if (< value 80)
		(generate-creature is_player value)
		(get-element spell_cards (random 4) t)
		)
	)

; Show the main information about the current game
(defun display-game-state ()
	(format t "~%#################################################################~%")
	
	(format t "Your Health: ")
	(write player_health)
	(format t  "~%Your Energy: ")
	(write player_energy)
	(format t "~%Cards on your Hand: ")
	(write Hand)

	(format t "~%~%********BOARD*********~%")
	(format t  "~%Your Cards on the Board: ")
	(write player_board)

	(format t  "~%~%Opponent's Cards on the Board: ")
	(write AI_board)
	(format t "~%**********************~%")

	(format t "~%~%Opponent's Number of Cards: ")
	(write (length AIHand))
	(format t "~%~%Opponent's Health: ")
	(write AI_health)

	(format t "~%#################################################################~%")
	)

; Execute the action through the spell card in the parameter
(defun use-spell (card isp)
	(if (string= "small_healer" (car(cdr (cdr card))))
		(progn
			(if (null isp)
					(if (> (length AI_board) 0)
						(progn
						(setq index (+ 1 (random (length AI_board))))
						(loop for elem in AI_board
							for i from 1 to (length AI_board) do
							(if (eq i index)
								(setq selected elem)
								)
							)
						(setq nh (* 1.2 (car (cdr (cdr (cdr elem))))))
						(setq ncard (cons (car elem) (cons (car (cdr elem)) (cons (car (cdr (cdr elem))) 
						(cons nh nil)))))
						(setq AI_board (substitute ncard elem AI_board :count 1))
						)
					)
					(if (< (length player_board) 0)
					(progn
						(setq index (+ 1 (random (length player_board))))
						(loop for elem in player_board
							for i from 1 to (length player_board) do
							(if (eq i index)
								(setq selected elem)
								)
							)
						(setq nh (* 1.2 (car (cdr (cdr (cdr elem))))))
						(setq ncard (cons (car elem) (cons (car (cdr elem)) (cons (car (cdr (cdr elem))) 
						(cons nh nil)))))
						(setq player_board (substitute ncard elem player_board :count 1))
						)
					)
				)
			)
		(if (string= "big_healer" (car(cdr (cdr card))))
			(progn
				(if (null isp)
						(if (< (length AI_board) 0)
							(progn
							(setq index (+ 1 (random (length AI_board))))
							(loop for elem in AI_board
								for i from 1 to (length AI_board) do
								(if (eq i index)
									(setq selected elem)
									)
								)
							(setq nh (* 1.5 (car (cdr (cdr (cdr elem))))))
							(setq ncard (cons (car elem) (cons (car (cdr elem)) (cons (car (cdr (cdr elem))) 
							(cons nh nil)))))
							(setq AI_board (substitute ncard elem AI_board :count 1))
							)
						)
						(if (< (length player_board) 0)
							(progn
							(setq index (+ 1 (random (length player_board))))
							(loop for elem in player_board
								for i from 1 to (length player_board) do
								(if (eq i index)
									(setq selected elem)
									)
								)
							(setq nh (* 1.5 (car (cdr (cdr (cdr elem))))))
							(setq ncard (cons (car elem) (cons (car (cdr elem)) (cons (car (cdr (cdr elem))) 
							(cons nh nil)))))
							(setq player_board (substitute ncard elem player_board :count 1))
							)
						)
					)
				)
			(if (string= "double_attack" (car(cdr (cdr card))))
				(progn
					(if (not (null isp))
						(if (< (length AI_board) 0)
							(progn
								(setq index (+ 1 (random (length AI_board))))
								(loop for elem in AI_board
									for i from 1 to (length AI_board) do
									(if (eq i index)
										(setq selected elem)
										)
									)
								(setq temp_card (cons 0 (cons "-" (cons 3 (cons 0 nil)))))
								(attacking selected temp_card (not isp))

								(setq index (+ 1 (random (length AI_board))))
								(loop for elem in AI_board
									for i from 1 to (length AI_board) do
									(if (eq i index)
										(setq selected elem)
										)
									)
								(setq temp_card (cons 0 (cons "-" (cons 3 (cons 0 nil)))))
								(attacking selected temp_card (not isp))
								)
							)
						(if (< (length player_board) 0)
							(progn
								(setq index (+ 1 (random (length player_board))))
								(loop for elem in player_board
									for i from 1 to (length player_board) do
									(if (eq i index)
										(setq selected elem)
										)
									)
								(setq temp_card (cons 0 (cons "-" (cons 3 (cons 0 nil)))))
								(attacking selected temp_card (not isp))

								(setq index (+ 1 (random (length player_board))))
								(loop for elem in player_board
									for i from 1 to (length player_board) do
									(if (eq i index)
										(setq selected elem)
										)
									)
								(setq temp_card (cons 0 (cons "-" (cons 3 (cons 0 nil)))))
								(attacking selected temp_card (not isp))
								)
							)
						)
					)
				(if (string= "extra_attack" (car(cdr (cdr card))))
					(progn
						(if (not (null isp))
							(if (< (length player_board) 0)
							(progn
								(setq index (+ 1 (random (length AI_board))))
								(print index)
								(loop for elem in AI_board
									for i from 1 to (length AI_board) do
									(if (eq i index)
										(setq selected elem)
										)
									)
								(setq temp_card (cons 0 (cons "-" (cons 4 (cons 0 nil)))))
								(attacking selected temp_card (not isp))
								)
							)
							(if (< (length player_board) 0)
							(progn
								(setq index (+ 1 (random (length player_board))))
								(loop for elem in player_board
									for i from 1 to (length player_board) do
									(if (eq i index)
										(setq selected elem)
										)
									)
								(setq temp_card (cons 0 (cons "-" (cons 4 (cons 0 nil)))))
								(attacking selected temp_card (not isp))
								)
							)
							)
						)
					)
				)
			)
		)
	)

; Put the card in the field.
(defun add-to-board (index isp)
	(if (null isp)
		(progn
			(loop for elem in AIHand
				for i from 1 to (length AIHand) do
				(if (eq i index)
					(setq selected_ elem)
					)
				)
			(if (stringp (car (cdr selected_)))
				(if (<= 0 (- AI_energy (car selected_)))
					(progn
						(setq AI_board (cons selected_ AI_board))
						(setq AIHand (remove selected_ AIHand :count 1))
						(setq AI_energy (- AI_energy (car selected_)))
						)
					(format t "~%Energy Insufficient!!")
					)
				(progn
					(use-spell selected_ isp)
					(setq AIHand (remove selected_ AIHand :count 1))
					(setq AI_energy (- AI_energy (car selected_)))
					)
				)
			)
		(progn
			(loop for elem in Hand
				for i from 1 to (length Hand) do
				(if (eq i index)
					(setq selected_ elem)
					)
				)
			(if (stringp (car (cdr selected_)))
				(if (<= 0 (- player_energy (car selected_)))
					(progn
						(setq player_board (cons selected_ player_board))
						(setq Hand (remove selected_ Hand :count 1))
						(setq player_energy (- player_energy (car selected_)))
						)
					(format t "~%Energy Insufficient!!")
					)
				(progn
					(use-spell selected_ isp)
					(setq Hand (remove selected_ Hand :count 1))
					(setq player_energy (- player_energy (car selected_)))
					)
			)
		)
		)
	)

; Get two cards, execute the attack, and refresh the board
(defun attacking (trgt attacker tisp)
	(setq ncard (cons (car trgt) (cons (car (cdr trgt)) (cons (car (cdr (cdr trgt))) 
	(cons (- (car (cdr (cdr (cdr trgt)))) (car (cdr (cdr attacker)))) nil)))))
	(if (<= (- (car (cdr (cdr (cdr trgt)))) (car (cdr (cdr attacker)))) 0)
		(if (null tisp)
			(setq AI_board (remove trgt AI_board :count 1))
			(setq player_board (remove trgt player_board :count 1))
			)
		(if (null tisp)
			(setq AI_board (substitute ncard trgt AI_board :count 1))
			(setq player_board (substitute ncard trgt player_board :count 1))
			)
		)
	)
; Find out which cards are in combat through the indexes
(defun make-attack (shooter sisp target tisp)
	(if (null sisp)
		(loop for elem in AI_board
			for i from 1 to (length AI_board) do
			(if (eq i shooter)
				(setq attacker elem)
				)
			)
		(loop for elem in player_board
			for i from 1 to (length player_board) do
			(if (eq i shooter)
				(setq attacker elem)
				)
			)
		)
	(if (null tisp)
		(loop for elem in AI_board
			for i from 1 to (length AI_board) do
			(if (eq i target)
				(setq trgt elem)
				)
			)
		(loop for elem in player_board
			for i from 1 to (length player_board) do
			(if (eq i target)
				(setq trgt elem)
				)
			)
		)
	(if (eq target 0)
		(if (null tisp)
			(setq AI_health (- AI_health (car (cdr (cdr attacker)))))
			(setq player_health (- player_health (car (cdr (cdr attacker)))))
			)
		(attacking trgt attacker tisp)
		)
	)

; Get a command, parse it and forward to the right function
(defun make-move (cmd isp)
	(setq cmd_list (coerce cmd 'list))
	(if (eq #\d (car cmd_list))
		(if (null isp)
			(if (< (length AIHand) 7)
				(setq AIHand (cons(create-card t) AIHand))
				)
			(if (< (length Hand) 7)
				(setq Hand (cons(create-card t) Hand))
				(format t "~%Your Hand is FULL!")
				)
			)
		(if (eq #\u (car cmd_list))
			(add-to-board (digit-char-p (car(cdr cmd_list))) isp)
			(if (eq #\a (car cmd_list))
				(make-attack (digit-char-p (car (cdr cmd_list))) isp (digit-char-p(car (cdr (cdr cmd_list)))) (not isp))
				(if (eq #\p (car cmd_list))
					(if (null isp)
						(setq AI_energy 0)
						(setq player_energy 0)
						)
					(if (eq #\q (car cmd_list))
						(quit)
						(if (eq #\h (car cmd_list))
							(progn
								(format t "~%~%~%~%~%~%*INSTRUCTIONS*~%~%If the monster is already on the field:~%")
								(format t "Type 'a' to attack and the number of the card that you want to play followed by the number of the card that you want to attack (without spaces).")
								(format t "Remember: 0 is to attack your opponent")
								(format t "~%If the monster or the spell card is on your hand:")
								(format t "~%Type 'u' (for use) and the number of the card (without spaces), and it will be put in the field.")
								(format t "~%If you want to draw a card:")
								(format t "~%Type 'd'")
								(format t "~%If you want to pass")
								(format t "~%Type 'p'")
								(format t "~%If you want to quit")
								(format t "~%Type 'q'")
								)
							(format t "~%Invalid command")
						)
						)
					)
				)
			)
		)
	)
; Execute all the Opponent's commands. They are:
; 1. Always draw a card
; 2. Just attack if there is more than 2 monsters in the board (randomly)
; 3 If there is less than 2 mosnters in the table, play one.
(defun AI-command ()
	(if (or (null AI_board) (< (length AI_board) 3))
		(progn
			(setq index nil)
			(loop for elem in AIHand
				for i from 1 to (length AIHand) do
				(if (> AI_energy (car elem))
					(setq  index i)
					)
				)
			(if (not(null index))
				(progn
					(setq AIcmd (concatenate 'string "u" (string (digit-char index))))
					(print AIcmd)
					(make-move AIcmd nil)
					)
				)
			(format t "~%Draw card")
			(make-move "d" nil)
			(format t "~%Passed")
			(make-move "p" nil)
			)
		(progn
			(format t "~%Attack")
			(setq AIcmd (concatenate 'string  "a" (string (digit-char (+ 1(random (length AI_board))))) (string (digit-char (random (length AI_board))))))
			(print AIcmd)
			(make-move AIcmd nil)
			(if (< (length AIHand) 7)
				(progn
					(format t "~%Draw card")
					(make-move "d" nil)
					)
				)
			(format t "~%Passed")
			(make-move "p" nil)
			)
		)
	)

; ################ INITIAL DEFINITIONS ################
; Ask name
(format t "~%Name: ")
(setq player_name (read-line))

(setq player_health 30)
(setq AI_health 30)
(setq playerp t)
(setq Hand (cons(create-card playerp) (cons (create-card playerp) (cons (create-card playerp) (cons (create-card playerp) nil)))))
(setq playerp nil)
(setq AIHand (cons(create-card playerp) (cons (create-card playerp) (cons (create-card playerp) (cons (create-card playerp) nil)))))

; ################ GAME LOOP (MAIN) ################
(make-move "h" t)
(setq player_board ())
(setq playerp nil)
(setq AI_board ())
; set turn to 1
(setq turn 1)
; repeat forever
(setq game_state t)
(loop
;    set player energy to equal turn number
(setq player_energy turn)
;    print game state
(display-game-state)
;    while player is not finished do  ; PLAYER TURN (read-eval-print loop)
(loop
	;       read player command
	(format t "~%Type your command (Press h for HELP): ")
	(setq command (read-line))
	(make-move command t)
	(display-game-state)
	;       if Player is dead, print message and exit
	(if (<= player_health 0)
		(progn
			(format t "~%LOSER")
			(setq game_state nil)
			)
		)
	;       if AI is dead, print message and exit
	(if (<= AI_health 0)
		(progn
			(format t "~%WINNER")
			(setq game_state nil)
			)
		)

		(when (<= player_energy 0) (return player_energy))
	)
	;       print game state
	(display-game-state)
	(format t "~%Computer's Turn~%")
	;    set AI energy to equal turn number
	(setq AI_energy turn)
	;    while AI is not finished to	    ; AI TURN
	(loop
		(AI-command )
		;       if Player is dead, print message and exit
		(if (<= player_health 0)
			(progn
				(format t "~%LOSER")
				(setq game_state nil)
				)
			)
		;       if AI is dead, print message and exit
		(if (<= AI_health 0)
			(progn
				(format t "~%WINNER")
				(setq game_state nil)
				)
			)
		(when (<= AI_energy 0) (return AI_energy))
	)
	;    increment turn
	(setq turn (+ turn 1))
	(when (null game_state) (return 0))
)