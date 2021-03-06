(defun member (x liste) 
   (cond
      ((null liste) ()) 
      ((equal (car liste) x) liste) 
      (t (member x (cdr liste))))) 

(defun inclus (liste1 liste2) 
   (cond 
      ((null liste1) t) 
      ((member (car liste1) liste2)(inclus (cdr liste1) liste2)) 
      (t ()))) 

(defun compare (liste1 liste2)
   (if ((and (inclus liste1 liste2) (inclus liste2 liste1)))
      (print "the 2 lists are equivalent")
      (print "the 2 lists aren't equivalent"))) 
