;state = ( 'nb de missionaires sur la rive gauche' 'nb de cannibales sur la rive gauche' 'coté de la rive où il y a le bateau')
(setq R-ops '((0 1) (1 0) (1 1) (2 0) (0 2)) )       ; <-
(setq L-ops '((0 -1) (-1 0) (-1 -1) (-2 0) (0 -2)) ) ; ->

(defun apply-op (state op)
	(cons (+ (car state) (car op)) (cons (+ (cadr state) (cadr op)) (if (eq (caddr state) 'R) 'L 'R)))
)

;parcours en profondeur
(defun solve (state previous-states)
	(print state)
	(cond
		((equal state '(0 0 R))
			(print (cons state previous-states)))
		((or (< (car state) 0) (< (cadr state) 0) nil))
		((or (> (car state) 3) (> (cadr state) 3) nil))
		( T (dolist (op (if (eq (caddr state) 'R) R-ops L-ops))
			(unless (find (apply-op state op) previous-states)
				(solve (apply-op state op) (cons (apply-op state op) previous-states))
			)
		))
	)
)
(print (solve '(3 3 L) '()))