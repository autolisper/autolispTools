;only cut first select object
(defun c:filletcut ( / r f s)
  ;get r 
  (setq r (getreal "r:"))
  ;select first
  (setq f (entsel))
  ;select second
  (setq s (entsel))
  ;copy first
  (vla-copy (vlax-ename->vla-object (car s)))
  ;fillet  
  (command-s "._fillet" "r" r)
  (command-s "._fillet" f s)
  ;del original second
  (vla-delete (vlax-ename->vla-object (car s)))
)
(defun HL:canfillet ( r e0 e1 p / )
  ;0
)
(defun HL:outerfillet ( cdata / e0 e1 r l e0c sp ep nsp nep re)
  (setq e0 (car cdata)
        e1 (cadr cdata)
        r (caddr cdata)
        l (cadddr cdata)
        )
  ;copy e0 -> e0c
  (vla-copy (vlax-ename->vla-object (car e0)))
  (setq e0c (list (entlast) (cadr e0)))
  ;save previous sp ep
  (setq sp (vlax-curve-getstartpoint (car e0)))
  (setq ep (vlax-curve-getstartpoint (car e0)))
  ;lengthen e0
  (command "._lengthen" "DE" l e0 "")
  ;get changed point and revise e0
  (setq nsp (vlax-curve-getstartpoint (car e0)))
  (setq nep (vlax-curve-getstartpoint (car e0)))
  (if (HL:eqp sp nsp)
      (setq e0 (list (car e0) nep))
      (setq e0 (list (car e0) nsp))
  )
  ;fillet e0 e1
  (command-s "._fillet" "r" r)
  (command-s "._fillet" e0 e1)
  (setq re (entlast))
  ;delete e0
  (vla-delete (vlax-ename->vla-object (car e0)))
  ;extend e0c  to created re
  (command-s "._extend" re "" e0c "")

)
(defun c:efillet ( / e0 e1 p r cdata)
  (setq r (getreal))
  (setq e0 (entsel))
  (setq e1 (entsel))  
  (setq p (getpoint))
  ;if canfillet
  (setq cdata (canfillet r e0 e1 p))
  (if cdata
    (if (= (car cdata) 0)
        (progn
          (command-s "._fillet" "r" r)
          (command-s rorc p0 p1) 
        )
        (HL:outerfillet (cdr cdata))
    )
  )
)