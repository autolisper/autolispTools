(setq HL:TAPERPROJECTIONSPLITNUM 100)
(defun HL:projection (p a innerpoint / p0 p1 center0 r0 center1 r1 e eright eleft z0 z1 interpoints ansp1 ansp0 tp)
  (setq p0 (car p))
  (setq p1 (cdr p))
  (setq z0 (caddr p0))
  (setq z1 (caddr p1))  
  (if (> z0 z1)
      (progn
        (setq tp p0)
        (setq p0 p1)
        (setq p1 tp)
        (setq tp z0)
        (setq z0 z1)
        (setq z1 tp)
      )
  )
  (setq tana (tan (HL:deg2rad a)))
  ;center0
  (setq center0 (list (car p0) (cadr p0)))
  (setq center1 (list (car p1) (cadr p1)))
  (setq r0 (HL:getD center0 center1))
  (setq r1 (* (- z1 z0) tana))
  (if (HL:eqr r1 0.0)
      (setq interpoints (cons center1 center1))
      (setq interpoints (HL:getCircleIntersectPoints center0 r0 center1 r1))
  )  
  (setq left (car interpoints))
  (setq right (cdr interpoints))
  (setq e (HL:lengthV (HL:-L center1 center0) (* z0 tana)))
  (setq eright (HL:rotateright90 e))
  (setq eleft (HL:rotateleft90 e))
  (if (> 0.0 (HL:outerproduct (HL:-L center1 center0) (HL:-L innerpoint center0)))
      (progn
        (setq ansp0 (HL:+L center0 eright))
        (setq ansp1 (HL:+L right eright))
        (cons ansp0 ansp1)
      )
      (progn
        (setq ansp0 (HL:+L center0 eleft))
        (setq ansp1 (HL:+L left eleft))
        (cons ansp0 ansp1)
      )
  )  
)
(defun HL:getCircleSplitPoints ( o / num start end points)
  (setq num HL:TAPERPROJECTIONSPLITNUM)
  (setq end (vlax-curve-getendparam o))
  (setq start (vlax-curve-getstartparam o))
  (setq ds (/ (- end start) num))
  (setq i 0)
  (repeat num
    (setq points (cons (vlax-curve-getpointatparam o (+ start (* ds i))) points)) 
    (setq i (1+ i))
  )
  (setq points (cons (vlax-curve-getendpoint o) points))
  (reverse points)
)
(defun HL:getPolySplitPoints ( o / num start end points)
  (setq end (vlax-curve-getendparam o))
  (setq start (vlax-curve-getstartparam o))
  (setq num (fix (- end start)))
  (setq ds (/ (- end start) num))
  (setq i 0)
  (repeat num
    (setq points (cons (vlax-curve-getpointatparam o (+ start (* ds i))) points)) 
    (setq i (1+ i))
  )
  (setq points (cons (vlax-curve-getendpoint o) points))
  (reverse points)  

)
(defun HL:getObjectPoints (o / otype ans)  
  (setq otype (HL:getType o))
  (cond
    ((= otype "LINE") (list (vlax-curve-getstartpoint o) (vlax-curve-getendpoint o)))
    ((= otype "POLYLINE") (HL:getPolySplitPoints o))
    ((= otype "LWPOLYLINE") (HL:getPolySplitPoints o))
    (T (HL:getCircleSplitPoints o))
  )  
)
(defun HL:_taperprojectionObject (object innerpoint a zoffset / points pointpairs)  
  (setq zoffset (list 0.0 0.0 zoffset))
  ;get points from object
  (setq points (HL:getObjectPoints object))  
  ;zoffset 
  (setq points (mapcar '(lambda (p) (HL:-L p zoffset)) points))
  (setq pointpairs (HL:makepair points))  
  ;change points to projection point
  (setq pointpairs (mapcar '(lambda (p) (HL:projection p a innerpoint)) pointpairs))
  ;draw lines
  (mapcar '(lambda (pair) (HL:drawLine (car pair) (cdr pair))) pointpairs)
)
(defun c:taperprojection ( / objects a zoffset object points pointpairs innerpoint)  
  (if (not HL:ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (HL:A_start)
  ;get Objects
  (while (not objects)
    (setq objects (ssget '((0 . "ARC,CIRCLE,LINE,LWPOLYLINE,SPLINE,ELLIPSE,POLYLINE"))))
  )    
  (setq objects (HL:getNameList objects))
  (while (not innerpoint)
    (setq innerpoint (getpoint "innerpoint:"))
  )
  (while (not a)
    (setq a (getreal "angle:"))
  )
  ;get zoffset
  (while (not zoffset)
    (setq zoffset (getreal "zoffset:"))
  )
  ;for objects
  (foreach object objects        
    ;change points to projection point and drawLines
    (HL:_taperprojectionObject object innerpoint a zoffset)
  )          
  (HL:A_end)
)