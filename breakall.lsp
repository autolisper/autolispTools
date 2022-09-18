;;must load utility.lsp at first

(defun isEndPoint (p ename / s e)
  (if (vlax-curve-isClosed ename)
      nil
      (progn
        (setq s (vlax-curve-getStartPoint ename))
        (setq e (vlax-curve-getEndPoint ename))
        (if (and s e)
            (or (eqp p s) (eqp p e))
            nil
        )  
      )
  )
)
(defun getEnamePointOn (enames point / ename ret)  
  (foreach ename enames
    (if (not ret)
      (if (vlax-curve-getDistAtPoint ename point)
          (setq ret ename)
      )
    )
  )
  ret
)
;;break closed
(defun breakCircle (ename p0 p1 / elist)
  (setq center (A_dxf 10 ename))
  (command-s "._arc" "C" center p0 p1)
  (setq elist (cons (entlast) elist))
  (command-s "._arc" "C" center p1 p0)
  (setq elist (cons (entlast) elist))
  (vla-delete (vlax-ename->vla-object ename))
  elist  
)
(defun breakPolyLineClosed (ename p0 p1)
  (princ "closed polyline cut is not implemented")
  nil
)
(defun breakSplineClosed (ename p0 p1)
  (princ "closed spline cut is not implemented")
  nil
)
(defun breakEllipse (ename p0 p1 / e0 sp ep )    
  (setq sp (vlax-curve-getparamatpoint ename p0))
  (setq ep (vlax-curve-getparamatpoint ename p1))  
  (entmod (U_dxf 42 ep (U_dxf 41 sp ename)))
  (entmake (U_dxf 41 ep (U_dxf 42 sp ename)))
  (list ename (entlast))
)
(defun breakclosed (ename p0 p1 / etype center)  
  (setq etype (A_dxf 0 ename))          
  (cond 
    ((= etype "CIRCLE") (breakCircle ename p0 p1))
    ((= etype "LWPOLYLINE")(breakPolyLineClosed ename p0 p1))
    ((= etype "SPLINE") (breakSplineClosed ename p0 p1))
    ((= etype "ELLIPSE") (breakEllipse ename p0 p1))
  )
)
(defun breakpoint ( ename pl / oldosmode)
  (setq oldosmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq pl (vl-remove-if '(lambda (p) (isEndPoint p ename)) pl))
  (if pl
    (_breakpoint (list ename) pl)
  )
  (setvar "OSMODE" oldosmode)
)
(defun _breakpoint ( enames pl / point ename)  
  (setq ename (car enames))  
  (if
    (not (vlax-curve-isClosed ename))
    (progn                  
      (foreach point pl
        ;;select 
        (setq ename (getEnamePointOn enames point))
        (command-s "._break" ename point point)
        (setq enames (cons (entlast) enames))                   
      )             
    )
    (progn      
      (if (< 1 (length pl))
        (progn          
          (setq enames (breakclosed ename (car pl) (cadr pl)))
          (if enames
            (progn
              (setq pl (cddr pl))
              (if pl            
                (_breakpoint enames pl)
              )
            )
          )
        )      
      )
    )
  )
)
(defun getIntersectPoints (ename0 ename / intPoints tempPoint pl)
  (setq intPoints (vla-IntersectWith (vlax-ename->vla-object ename0) (vlax-ename->vla-object ename) acExtendNone))         
  (if (/= (type intPoints) vlax-vbEmpty)        
      (progn
        (setq tempPoint (vlax-safearray->list (vlax-variant-value intPoints)))  ;交点リストを取得
        (while tempPoint
          (setq
            pl (cons (list (car tempPoint) (cadr tempPoint) (caddr tempPoint)) pl)
            tempPoint (cdddr tempPoint)
          )
        )
      )
  )
  pl
)
(defun breakobject ( cutename ename / intPoints tempPoint pl)  
  (setq pl (getIntersectPoints cutename ename))
  (if pl
      (breakpoint ename pl)
  )
)
    
(defun _breakall ( fp sp sset / doc modelSpace lineObj intPoints enameList ename lineE)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))  
  (setq fp (vlax-3d-point fp)
          sp (vlax-3d-point sp))   
  (setq modelSpace (vla-get-ModelSpace doc))  
  (setq lineObj (vla-AddLine modelSpace fp sp))
  (setq lineE (vlax-vla-object->ename lineObj))
  (setq enameList (getNameList sset))
  (foreach ename enameList
    (breakobject lineE ename)
  )
  (vla-delete lineObj)
)
(defun c:breakall ( / fp sp sset oldsmode)
  (A_start)
  (setq fp nil)
  (setq sp nil)
  (while (or (not fp) (not sp) (eqp fp sp))
  ;pick first point
    (while (not (setq fp (getpoint "first point:")))
      (princ "\n")
    )
    (princ "\n")
    (if fp
      (progn
        ;pick second point
        (while (not (setq sp (getpoint "second point:" fp)))
          (princ "\n")
        )        
        (princ "\n")        
      )
    )
  )    
  (setq sset (ssget "F" (list fp sp) '((0 . "ARC,CIRCLE,LINE,LWPOLYLINE,SPLINE,ELLIPSE"))))  
  (_breakall fp sp sset)    
  (A_end) 
)