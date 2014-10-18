(defun createList(lst)
(cond	
	((eq lst nil) (setq lst (list 'a)) (createlist lst))
	(t (cons (car lst) '(b)))
    )
)
