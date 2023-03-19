;;must load utility.lsp at first


;;lenghth is arbitary
(defun HL:getNormal (ename point point2 / normal)
  (cond ( 
    (= (HL:A_dxf 0 ename) "LINE")          
    (setq normal (HL:rotateright90 (HL:-L (vlax-curve-getendPoint ename) (vlax-curve-getstartPoint ename))))              
    )
    (
    (member (HL:A_dxf 0 ename) '("ARC" "CIRCLE"))          
    (setq normal (HL:-L point (HL:A_dxf 10 ename)))              
    )
  )
  (if normal
    (progn
      (if (< (HL:innerproduct normal (HL:-L point2 point)) 0)
        (setq normal (list (HL:neg (car normal)) (HL:neg (cadr normal)) 0))
        (setq normal (list (car normal) (cadr normal) 0))
      )    
    )    
  )  
  normal
)
(defun c:tangentpointcircle ( / doc modelSpace point r ename normal point2 center $_device $_code $data flag circleObj)
  (if (not HL:ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (HL:A_start)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))  
  (setq modelSpace (vla-get-ModelSpace doc))  
  (while (not ename)
    (setq ename (car (entsel "select object:")))        
    (princ "\n")
    (if (not (member (HL:A_dxf 0 ename) '("LINE" "CIRCLE" "ARC")))
      (setq ename nil)      
    )
  )
  (while (not point)
    (setq point (getpoint "contact point:"))
    (princ "\n")
    (if point
      (setq point (vlax-curve-getclosestpointto ename point))
    )
  )
  (while (not r)
    (setq r (getreal "radius:"))
    (princ "\n")    
    (if (<= r 0)
        (setq r nil)
    )    
  )  
  (setq point2 point)
  (setq normal (HL:getNormal ename point point2))
  (if normal
    (progn
      (setq normal (HL:lengthV normal r))
      (setq center (HL:+L point normal))
      (setq center (vlax-3d-point center))
      (setq circleObj (vla-AddCircle modelSpace center r))
      (prompt "click which side:\n")
      (while (not flag)
        (setq $_device (grread T (+ 1 2 4 8) 0)
              $_code   (car $_device)
              $_data   (cadr $_device)
        )
        (if (or (= $_code 5) (= $_code 3))                   ;Mouse Move
            (progn
              (setq point2 $_data)        
              (if (> 0 (HL:innerproduct normal (HL:-L point2 point)))
                (progn
                  (setq normal (HL:-L (list 0 0 0) normal))
                  (setq center (HL:+L point normal))
                  (setq center (vlax-3d-point center))
                  (vla-put-center circleObj center)
                  (vla-update circleObj)    
                )  
              )
              (if (= $_code 3)
                  (setq flag T)
              )
            )
        )        
      )
    )
  )    
  (HL:A_end)
)