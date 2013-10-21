;state = ( 'nb de missionaires sur la rive gauche' 'nb de cannibales sur la rive gauche' 'coté de la rive où il y a le bateau')
(setq R-ops '((0 1) (1 0) (1 1) (2 0) (0 2)) )       ; <-
(setq L-ops '((0 -1) (-1 0) (-1 -1) (-2 0) (0 -2)) ) ; ->

(defun apply-op (state op)
	((+ (car state) (car op)) (+ (cadr state) (cadr op)) (if (eq (caddr state) 'R) 'L 'R))
)

(defun solve (state previous-states)
	(cond
		((equal state '(0 0 R))
			(print (cons state previous-states))
		((or (< (car state) 0) (< (cadr state) 0)) nil)
		(dolist (op (if (eq (caddr state) 'R) R-ops L-ops))
			(unless (list-contains previous-states (apply-op state op))
				(solve (apply-op state op) (cons (apply-op state op) previous-states))
			)
		)
)
(print (solve '(3 3 L) '()))