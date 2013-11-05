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
(defun BFS-solve (Q)
	(dolist (node Q)
		(setq Q (dequeue Q))
		(cond 
			((equal (car node) '(0 0 0)) (print "Solution:") (print (append (cadr node) (list '(0 0 0)))))  
			((verify (car node)) 
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
	(if (null Q) "END" (BFS-solve Q))	
)	

(defun init-solve (state)
	(let ((Q (enqueue (list state '()) '())))
		(BFS-solve Q)
	)	
)

;parcours en profondeur
(defun DFS-solve (state previous-states)
	(cond
		((equal state '(0 0 0))
			(print "Solution:") (print (nconc previous-states (list '(0 0 0)))))
		((not (verify state)) nil)
		;appliquer toutes les transformations possibles
		(T
            (dolist (op (if (eq (caddr state) 0) *R-ops* *L-ops*))
                (setq res (apply-op state op))
                ;verifier que l'on ne retourne pas dans un état déjà 
                (if (and (not (in res previous-states)) (verify res))
                    (if (null previous-states)
                        (DFS-solve res (list state))
                        (DFS-solve res (nconc previous-states (list state)))
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
		((and (and (not (= (car state) 3)) (not (= (car state) 0)))
				(not (= (car state) (cadr state)))
			)nil)
		(T T)
	)
)

(print "Parcours en largeur") 
(init-solve '(3 3 1))
(print "") 
(print "Parcours en profondeur") 
(DFS-solve '(3 3 1) '())

;"Parcours en largeur" 
;"Solution:" 
;((3 3 1) (2 2 0) (3 2 1) (3 0 0) (3 1 1) (1 1 0) (2 2 1) (0 2 0) (0 3 1)
; (0 1 0) (0 2 1) (0 0 0)) 
;"Solution:" 
;((3 3 1) (2 2 0) (3 2 1) (3 0 0) (3 1 1) (1 1 0) (2 2 1) (0 2 0) (0 3 1)
; (0 1 0) (1 1 1) (0 0 0)) 
;"Solution:" 
;((3 3 1) (3 1 0) (3 2 1) (3 0 0) (3 1 1) (1 1 0) (2 2 1) (0 2 0) (0 3 1)
; (0 1 0) (0 2 1) (0 0 0)) 
;"Solution:" 
;((3 3 1) (3 1 0) (3 2 1) (3 0 0) (3 1 1) (1 1 0) (2 2 1) (0 2 0) (0 3 1)
; (0 1 0) (1 1 1) (0 0 0)) 
;"" 
;"Parcours en profondeur" 
;"Solution:" 
;((3 3 1) (2 2 0) (3 2 1) (3 2 1) (3 0 0) (3 1 1) (1 1 0) (2 2 1) (0 2 0)
; (0 3 1) (0 1 0) (0 2 1) (0 0 0)) 
;"Solution:" 
;((3 3 1) (3 1 0) (3 2 1) (3 2 1) (3 0 0) (3 1 1) (1 1 0) (2 2 1) (0 2 0)
; (0 3 1) (0 1 0) (0 2 1) (0 0 0)) 