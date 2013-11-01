;state = ( 'nb de missionaires du mau' 'nb de cannibales sur la rive gauche' 'nb de bateau sur la rive gauche')
(setq R-ops '((0 1) (1 0) (1 1) (2 0) (0 2)) )       ; <-
(setq L-ops '((0 -1) (-1 0) (-1 -1) (-2 0) (0 -2)) ) ; ->


(defun in (x L)
	(if (or (eq x nil) (eq L nil)) NIL (if (equal (car L) x) T (in x (cdr L))))
)

(defun apply-op (state op)
	(list (+ (car state) (car op)) (+ (cadr state) (cadr op)) (if (eq (caddr state) 1) 0 1))
)

;parcours en profondeur
(defun solve (state previous-states)
	(cond
		((equal state '(0 0 0))
			(print (nconc previous-states (list '(0 0 0)))))
		((not (verify state)) nil)
		;appliquer toutes les transformations possibles
		(T (let ((res '()))
				(dolist (op (if (eq (caddr state) 0) R-ops L-ops))
					(setq res (apply-op state op))
					;verifier que l'on ne retourne pas dans un état déjà 
					(if (and (not (in res previous-states)) (verify res))
						(if (null previous-states)
							(solve res (list state))
							(solve res (nconc previous-states (list state)))
						)
					)
				)
			)
		)
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

;TODO:parcours en largeur
(print (solve '(3 3 1) '()))
