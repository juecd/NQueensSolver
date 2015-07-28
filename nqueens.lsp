; State representation automatically prevents queens being in the same row
; Manually check for queens in the same column or diagonal

; ----- contains_attacks implementation ----- ;


; flip_board:
; Returns a board that is flipped horizontally in order to check for leftward-downward diagonals (by simply recalling row_contains repeated)
(defun flip_board(s len)
	(cond ((NULL s) nil)
			(t (append (list (+ (- len (first s)) 1)) (flip_board (rest s) len)))
		) ;end cond
	) ;end defun

; row_contains_repeated:
; Checks recursively if any other row contains the value in row one or if a rightward-downward diagonal contains queens
(defun row_contains_repeated(s val num_attacks)
	(cond ((NULL s) num_attacks)
			((= (first s) val) (row_contains_repeated (rest s) val (+ 1 num_attacks)))
			(t (row_contains_repeated (rest s) val num_attacks))
		) ;end cond
	) ;end defun

; diag_contains_repeated:
; Similar implementation to checking for row
; Only checks rightward-downward
(defun diag_contains_repeated(s check_val num_attacks)
	(cond ((NULL s) num_attacks)
			((= (first s) check_val) (diag_contains_repeated (rest s) (+ check_val 1) (+ 1 num_attacks)))
			(t (diag_contains_repeated (rest s) (+ check_val 1) num_attacks))
		) ;end cond
	)

; contains_attacks:
; Checks one by one for repeated columns in the state given
; Returns nil if does not contain repeated columns or attacks in the diagonals
; Else returns the total number of attacks
; NOTE: an attack is defined by being in the same row, column, or diagonal EVEN IF another queen is already in the way
(defun contains_attacks(s)
	(cond ((NULL s) 0)
		(t (+ (+ (+ (row_contains_repeated (rest s) (first s) 0) (diag_contains_repeated (rest s) (+ (first s) 1) 0)) (diag_contains_repeated (rest (flip_board s (length s))) (+ (first (flip_board s (length s))) 1) 0)) (contains_attacks (rest s)))
			) ;end t
		) ;end cond
	) ;end defun



; ----- end contains_attacks implementation ----- ;

; set-square:
; Returns a new state with the row specified changed to val based on orig_s
; Row_index represents the row to change
; Val represents the value of the row to change to (ie col number)
(defun set-square(orig_s row_index val)
	(cond ((NULL orig_s) nil)
		((< row_index 1) nil)
		((= row_index 1) (append (list val) (rest orig_s)))
		(t (append (list (first orig_s)) (set-square (rest orig_s) (- row_index 1) val)))
		)
	)

; try-move:
; Returns a list of new states by recursively "moving" a queen in a given row (row_num) one step right, then two steps, ... (- (length orig_s) 1) steps IF the move results in a lower number of attacking queens
; Row_num is an index representing the row to "move" the queen aka change its value (ie column number)
; Counter keeps tracks of the current column count to check for attacks
(defun try-move (orig_s row_num counter)
	(let ((moved_q (set-square orig_s row_num counter)))
		(cond ((> counter (length orig_s)) nil)
				((equal moved_q orig_s) (try-move orig_s row_num (+ counter 1)))
				(t
					(cond ((< (contains_attacks moved_q) (contains_attacks orig_s)) (append (list moved_q) (try-move orig_s row_num (+ counter 1))))
							(t (try-move orig_s row_num (+ counter 1))
								) ;end t
						) ;end cond
					) ;end t
			) ;end cond
		) ;end let
	)

; next_states:
; Returns potential states that meet constraint that new state must contain less attacks than original state
; Recursively calls try-move on each row
(defun next_states(orig_s row_num)
	(cond ((> row_num (length orig_s)) nil)
			(t (append (try-move orig_s row_num 1) (next_states orig_s (+ row_num 1)))
			) ;end t
		) ;end cond
	) ;end defun

; on-path:
; Checks for repeated states
; Same implementation as HW 2's on-path
(defun on-path (s states)
	(cond ((NULL states) nil)
			((equal (first states) s) 'T)
			(t (on-path s (rest states)))
		)
	)

; dfs_helper:
(defun dfs_helper (states path)
	(let ((x (dfs (first states) path)))
		(cond ((NULL states) nil) ; No solution
				((and (NULL x) (NULL (rest states))) nil)
				((NULL x) (dfs_helper (rest states) path))
				(t x)
			) ;end cond
		) ;end let
	) ;end defun

; dfs:
; Takes state s which is in the format of the solution (q1 q2 q3...) and runs dfs on it by changing only the columns (ie queen at row x's value)
(defun dfs(s path)
	(cond ((= 0 (contains_attacks s)) s) ; Check for goal state
			((on-path s path) nil)
			(t (dfs_helper (next_states s 1) (append path (list s)))
				) ;end t
		) ;end cond
	) ;end defun

; create_list
; Create a list with N elements all in column 1 to begin the search
(defun create_list(N)
	(cond ((= 0 N) nil)
		(t (append (list 1) (create_list (- N 1))))
		) ;end cond
	) ;end defun

; QUEENS:
; Entry-point that calls the dfs on a list that represents the state
(defun QUEENS (N)
	; Place N queens on column 1 with N rows
	(dfs (create_list N) nil)
	)