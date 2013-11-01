;state = ( 'nb de missionaires du mau' 'nb de cannibales sur la rive gauche' 'nb de bateau sur la rive gauche')
(setq *R-ops* '((0 1) (1 0) (1 1) (2 0) (0 2)))       ; <-
(setq *L-ops* '((0 -1) (-1 0) (-1 -1) (-2 0) (0 -2))) ; ->


(defun in (x L)
	(if (or (eq x nil) (eq L nil)) NIL (if (equal (car L) x) T (in x (cdr L))))
)

(defun apply-op (state op)
	(list (+ (car state) (car op)) (+ (cadr state) (cadr op)) (if (eq (caddr state) 1) 0 1))
)

(defun enqueue (x Q)
	(if (null Q)
		(list x)
		(nconc Q (list x))
	)
)

(defun dequeue (Q)
	(cdr Q)
)
;parcours en largeur
;Algorithm I may use a queue.
;solve-larg (state previous-states=())
;	;on applique les operations puis on invalide les etats impossibles
	;on verifie que les etats sont correctes par rapport aux nombre de missionaires et de cannibales.
	;on enfile les etats apres verification
	;ensuite on reapplique le meme principe.

(defun solve-larg (Q)
	(dolist (node Q)
		(setq Q (dequeue Q))
		(cond 
			((equal (car node) '(0 0 0)) (print "One of the solution is ") (print (append (cadr node) (list '(0 0 0)))))  
			((verify (car node)) 
				(let ((res '()))
					(dolist (op (if (eq (caddr (car node)) 0) *R-ops* *L-ops*))
						(setq res (apply-op (car node) op))
						(if (and (not (in res (cadr node))) (verify res))
							(if (eq (cadr node) NIL)	
								(setq Q (enqueue (list res (list (car node))) Q))
								(setq Q (enqueue (list res (append (cadr node) (list (car node)))) Q))
							)
						)	
					)
				)
			)
		)
	)
	(if (null Q) "END" (solve-larg Q))	
)	

(defun init-solve (state)
	(let ((Q (enqueue (list state '()) '())))
		(solve-larg Q)
	)	
)

(defun verify (state)
	(cond
		((or (< (car state) 0) (< (cadr state) 0)) nil)
		((or (> (car state) 3) (> (cadr state) 3)) nil)
		;verifie nb de sauvages supérieurs aux missionaires sur l'autre rive
		(
			(and 
				(and (not (= (car state) 3)) (not (= (car state) 0)))
				(not (= (car state) (cadr state)))
			)
			nil
		)
		(T T)
	)	
)

;pour test
(init-solve '(3 3 1))
