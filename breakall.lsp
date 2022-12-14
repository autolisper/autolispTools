;;must load utility.lsp at first
;;define breakbyobject and breakbyline

(defun HL:isEndPoint (p ename / s e)
  (if (vlax-curve-isClosed ename)
      nil
      (progn
        (setq s (vlax-curve-getStartPoint ename))
        (setq e (vlax-curve-getEndPoint ename))
        (if (and s e)
            (or (HL:eqp p s) (HL:eqp p e))
            nil
        )  
      )
  )
)
(defun HL:getEnamePointOn (enames point / ename ret)  
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
(defun HL:breakCircle (ename p0 p1 / elist center)
  (setq center (HL:A_dxf 10 ename))
  (command-s "._arc" "C" center p0 p1)
  (setq elist (cons (entlast) elist))
  (command-s "._arc" "C" center p1 p0)
  (setq elist (cons (entlast) elist))
  (vla-delete (vlax-ename->vla-object ename))
  elist  
)
(defun HL:_getCurveD (ename p0 p1)
  (+ (HL:getD (vlax-curve-getstartpoint ename) p0) (HL:getD (vlax-curve-getendpoint ename) p1))
)
(defun HL:getCurveD (ename p0 p1)
  (min (HL:_getCurveD ename p0 p1) (HL:_getCurveD ename p1 p0))
)
(defun HL:getClosestCurve (ename0 ename1 p0 p1)
  (if (< (HL:getCurveD ename0 p0 p1) (HL:getCurveD ename1 p0 p1))
      ename0
      ename1
  )
)
(defun HL:getOneOfBreakCurve (ename p0 p1 breakpoint0 breakpoint1 breakpoint2 / ename2 oename1)  

  (command-s "._break" ename breakpoint0 breakpoint1)    
  (command-s "._break" ename breakpoint2 breakpoint2)  
  ; (command "._break" ename)
  ; (command breakpoint0)
  ; (command breakpoint1)
  ; (command "._break" ename)
  ; (command breakpoint2)
  ; (command breakpoint2)    
  (setq ename2 (entlast))
  (setq oename1 (HL:getClosestCurve ename ename2 p0 p1))
  (if (equal oename1 ename)
    (vla-delete (vlax-ename->vla-object ename2))
    (vla-delete (vlax-ename->vla-object ename))
  )
  oename1  
)
(defun HL:breakPolyLineClosed (ename p0 p1 / sp ep tmp start end ename1 ename2 oename1 oename2)
  (setq sp (vlax-curve-getparamatpoint ename p0))
  (setq ep (vlax-curve-getparamatpoint ename p1))
  (setq start (vlax-curve-getstartparam ename))
  (setq end (vlax-curve-getendparam ename))
  (if (< ep sp)
      (progn
        (setq tmp sp)        
        (setq sp ep)
        (setq ep tmp)
        (setq tmp p0)        
        (setq p0 p1)
        (setq p1 tmp)
      )
  )
  (cond ((and (= sp start) (= ep end)) nil)
        ((= sp start) 
          (progn            
            (entmake (entget ename))
            (setq ename1 (entlast))
            (setq oename1 (HL:getOneOfBreakCurve ename p0 p1 (vlax-curve-getpointatparam ename (max sp (- ep 0.00000001) )) p1 p0))
            (setq oename2 (HL:getOneOfBreakCurve ename1 p0 p1 (vlax-curve-getpointatparam ename1 (min end (+ ep 0.00000001) )) p1 p0))
            (list oename1 oename2)            
          )
        )
        ;((= ep end) nil) same as below
        (T (progn
            (entmake (entget ename))
            (setq ename1 (entlast))
            (setq oename1 (HL:getOneOfBreakCurve ename p0 p1 (vlax-curve-getpointatparam ename (max start (- sp 0.00000001) )) p0 p1))
            (setq oename2 (HL:getOneOfBreakCurve ename1 p0 p1 (vlax-curve-getpointatparam ename1 (min ep (+ sp 0.00000001) )) p0 p1))
            (list oename1 oename2)            
           )
        )
  )
)
(defun HL:breakEllipse (ename p0 p1 / e0 sp ep )    
  (setq sp (vlax-curve-getparamatpoint ename p0))
  (setq ep (vlax-curve-getparamatpoint ename p1))  
  (entmod (HL:U_dxf 42 ep (HL:U_dxf 41 sp ename)))
  (entmake (HL:U_dxf 41 ep (HL:U_dxf 42 sp ename)))
  (list ename (entlast))
)
(defun HL:breakclosed (ename p0 p1 / etype center)  
  (setq etype (HL:A_dxf 0 ename))          
  (cond 
    ((= etype "CIRCLE") (HL:breakCircle ename p0 p1))
    ((= etype "LWPOLYLINE")(HL:breakPolyLineClosed ename p0 p1))
    ((= etype "SPLINE") (progn (princ "closed spline is not implemented") nil)); (breakPolyLineClosed ename p0 p1))s
    ((= etype "ELLIPSE") (HL:breakEllipse ename p0 p1))
  )
)
(defun HL:breakpoint ( ename pl / oldosmode)
  (setq oldosmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq pl (vl-remove-if '(lambda (p) (HL:isEndPoint p ename)) pl))
  (if pl
    (HL:_breakpoint (list ename) pl)
  )
  (setvar "OSMODE" oldosmode)
)
(defun HL:_breakpoint ( enames pl / point ename)  
  (setq ename (car enames))  
  (if
    (not (vlax-curve-isClosed ename))
    (progn                  
      (foreach point pl
        ;;select 
        (setq ename (HL:getEnamePointOn enames point))
        (command-s "._break" ename point point)
        (setq enames (cons (entlast) enames))                   
      )             
    )
    (progn      
      (if (< 1 (length pl))
        (progn          
          (setq enames (HL:breakclosed ename (car pl) (cadr pl)))
          (if enames
            (progn
              (setq pl (cddr pl))
              (if pl            
                (HL:_breakpoint enames pl)
              )
            )
          )
        )      
      )
    )
  )
)
(defun HL:breakobject ( cutename ename / intPoints tempPoint pl)  
  (setq pl (HL:getIntersectPoints cutename ename))
  (if pl
      (HL:breakpoint ename pl)
  )
)
    
(defun HL:_breakbyline ( fp sp sset / doc modelSpace lineObj intPoints enameList ename lineE)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))  
  (setq fp (vlax-3d-point fp)
          sp (vlax-3d-point sp))   
  (setq modelSpace (vla-get-ModelSpace doc))  
  (setq lineObj (vla-AddLine modelSpace fp sp))
  (setq lineE (vlax-vla-object->ename lineObj))
  (setq enameList (HL:getNameList sset))
  (foreach ename enameList
    (HL:breakobject lineE ename)
  )
  (vla-delete lineObj)
)
(defun c:breakbyobject ( / ename0 ename1 enamelist)
  (if (not HL:ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (HL:A_start)
  (while (not ename0)    
    (setq ename0 (car (entsel "select cutting object:")))
    (princ "\n")
  )  
  (while (not enamelist)    
    (setq enamelist (HL:getNameList (ssget  '((0 . "ARC,CIRCLE,LINE,LWPOLYLINE,SPLINE,ELLIPSE")))))     
  )
  (princ ename0)
  (foreach ename1 enamelist
    (if (not (equal ename0 ename1))
      (HL:breakobject ename0 ename1)
    )
  )
  (HL:A_end)
)
(defun c:breakbyline ( / fp sp sset oldsmode)
  (if (not HL:ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (HL:A_start)
  (setq fp nil)
  (setq sp nil)
  (while (or (not fp) (not sp) (HL:eqp fp sp))
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
  (HL:_breakbyline fp sp sset)    
  (HL:A_end) 
)