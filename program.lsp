
(defun printcolumns(m)
(loop for i from 0 to m do (princ "+")(princ " "))
)

(defun printboard(n m)
(loop for i from 0 to n do (printcolumns m)(terpri))
)

(defun run() 
(setq in-file (open "input.dat" :direction :input))
(setq row (read in-file))
(setq column (read in-file))
(printboard row column)
(close in-file)

)

(defun readfromfile()
(setq in-file (open "input.dat" :direction :input))
(setq row (read in-file))
(setq column (read in-file))
;(readfile in-file)
;(setq mylst (createfinallist  in-file row column 'nil))
;(princ mylst)
(makemylist in-file row column 'nil)
(close in-file)
)


(defun makemylist(in-file n m lst)

	(setq lst (append (list (createlist in-file m nil))))
	(loop for i from 1 to (1- n) do
	 (push (createlist in-file m nil) (cdr (last lst))) 
			
	)
	(princ lst)
)


(defun readfile (infile)
       (do ((num (read infile nil 'eof) (read infile nil 'eof)))
         ((eql num 'eof) 'done)   ;; End of loop test
         (format t "~a~%" num)
       )
   
)

#|
(defun createfinallist(in-file n m  flst))
(
	(cond
	((eq n 0) flst)	
	((eq flst nil) (setq flst ( list ())) (createfinallist in-file n m flst))
	(t (setq flst (append flst (list (setq mylist(createlist infile m 'nil ))))) (createfinallist infile (1-n) m flst) )
	)
)
|#


(defun createList(in-file m lst)
	(cond
		    ((eq m 0) lst)
		    ((eq lst nil) (setq lst (list (read in-file))) (createlist in-file (1- m) lst))			
		    (t (setq lst (append lst (list (read in-file)))) (createlist in-file (1- m) lst))
		    
	)
)

