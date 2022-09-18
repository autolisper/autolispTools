(defun getMillseconds ( )
  (getvar "MILLISECS")  
)
(defun movecircle ( / *actdoc* mspace pt1 mycircle i newcenter)
  (setq *actdoc* (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace *actdoc*))
  (setq pt1 (getpoint))  
  (setq mycircle (vla-addcircle mspace (vlax-3d-point pt1) 4.0))    
  (setq i 0)
  (setq start (getMillseconds))
  (setq now (getMillseconds))    
  (setq x (car pt1))
  (setq y (cadr pt1))
  (setq vx 10.0)
  (setq vy 10.0)
  (while (< (- now start) 3000)
    (setq pre now)  
    (setq now (getMillseconds))
    (setq sa (/ (- now pre) 1000.0))
    (setq vy (+ vy (* -9.8 sa)))
    (setq x (+ x (* vx sa)))
    (setq y (+ y (* vy sa)))
    (setq newcenter (list x y (caddr pt1)))
    (vla-put-center mycircle (vlax-3d-point newcenter))
    (vla-update mycircle)    
  )
)