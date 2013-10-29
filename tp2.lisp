;state = ( 'nb de missionaires sur la rive gauche' 'nb de cannibales sur la rive gauche' 'coté de la rive où il y a le bateau')
(setq R-ops '((0 1) (1 0) (1 1) (2 0) (0 2)) )       ; <-
(setq L-ops '((0 -1) (-1 0) (-1 -1) (-2 0) (0 -2)) ) ; ->


(defun list-contains (x L)
	(if (or (eq x nil) (eq L nil)) 'F (if (equal (car L) x) 'T (list-contains x (cdr L))))
)

(defun apply-op (state op)
	(list (+ (car state) (car op)) (+ (cadr state) (cadr op)) (if (eq (caddr state) 'R) 'L 'R))
)

;parcours en profondeur
(defun solve (state previous-states)
	(cond
		((equal state '(0 0 R))
			(print previous-states))
		((or (< (car state) 0) (< (cadr state) 0) nil))
		((or (> (car state) 3) (> (cadr state) 3) nil))
		( T (dolist (op (if (eq (caddr state) 'R) R-ops L-ops))
			(if (eq (list-contains (apply-op state op) previous-states) 'F)
				(solve (apply-op state op) (cons (apply-op state op) previous-states))
			)
		))
	)
)
(print (list-contains '(1 3) '( (1 2) (3 4))))
(print (solve '(3 3 L) '()))
