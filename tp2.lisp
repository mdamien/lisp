;state = ( 'nb de missionaires du mau' 'nb de cannibales sur la rive gauche' 'nb de bateau sur la rive gauche')
(setq R-ops '((0 1) (1 0) (1 1) (2 0) (0 2)) )       ; <-
(setq L-ops '((0 -1) (-1 0) (-1 -1) (-2 0) (0 -2)) ) ; ->


(defun list-contains (x L)
	(if (or (eq x nil) (eq L nil)) 'F (if (equal (car L) x) 'T (list-contains x (cdr L))))
)

(defun apply-op (state op)
	(list (+ (car state) (car op)) (+ (cadr state) (cadr op)) (if (eq (caddr state) 1) 0 1))
)

;parcours en profondeur
(defun solve (state previous-states)
	;(print state)
	;(print previous-states)
	(cond
		((equal state '(0 0 0))
			(print previous-states))
		;invalide les états invalides
		((or (< (car state) 0) (< (cadr state) 0) nil))
		((or (> (car state) 3) (> (cadr state) 3) nil))
		;TODO: verifier nb de sauvages supérieurs aux missionaires sur l'autre rive
		(
			(or (and (eq (caddr state) 1) (> (cadr state) (car state)))
			(and (eq (caddr state) 0) (< (cadr state) (car state)))
			)
			nil
		)
		;appliquer toutes les transformations possibles
		( T (dolist (op (if (eq (caddr state) 0) R-ops L-ops))
			;verifier que l'on ne retourne pas dans un état déjà 
			(if (eq (list-contains (apply-op state op) previous-states) 'F)
				(solve (apply-op state op) (cons (apply-op state op) previous-states))
			)
		))
	)
)

;TODO:parcours en largeur
(print (solve '(3 3 1) '()))
