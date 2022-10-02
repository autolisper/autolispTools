
(defun HL:getInputR ( / r )
  (setq r (getreal "length:"))
  (prompt "\n")  
  r
)

(defun HL:getPLineAndPoint ( / pline entp)
  (while (= pline nil)
    (setq entp (entsel "pick pline:"))
    (setq pline (car entp))
    (prompt "\n")
    (if (not (= (HL:A_dxf 0 pline) "LWPOLYLINE"))
        (setq pline nil)
    )
  )
  (cons pline (cadr entp))
)

(defun HL:getSide (pline vertex  / v fv outp)
  (while (= v nil)
    (setq v (getpoint "which side:"))
    (prompt "\n")
  )  
  (setq fv (vlax-curve-getfirstderiv pline (vlax-curve-getparamatpoint pline (vlax-curve-getclosestpointto pline vertex))))
  (setq outp (HL:outerproduct fv (HL:-L v vertex)))
  (if (< outp 0)
      -1
      1
  )
)
(defun HL:plineExplodeAndGetElementVertexAndPair ( pline / tL retL isclosed retlist p)
  (setq isclosed (vlax-curve-isclosed (vlax-ename->vla-object pline)))
  (command-s "._explode" pline)
  (setq tL (ssget "p"))
  (setq i 0)
  (repeat (1- (sslength tL)) 
    (setq e0 (ssname tL i))
    (setq e1 (ssname tl (1+ i)))
    (setq p (vlax-curve-getstartpoint e1))
    (setq retlist (cons (cons p (cons e0 e1)) retlist))
    (setq i (1+ i))
  )
  (if isclosed
      (progn
        (setq e0 (ssname tL (1- (sslength tL)) ))
        (setq e1 (ssname tL 0))
        (setq p (vlax-curve-getstartpoint e1))
        (setq retlist (cons (cons p (cons e0 e1)) retlist))        
      )
  )  
  (reverse retlist)
)
;TODO
(defun HL:isSide (v p0 p1 side / fv0 fv1 outp) 
  (setq fv0 (vlax-curve-getfirstderiv p0 (vlax-curve-getparamatpoint p0 (vlax-curve-getclosestpointto p0 v))))
  (setq fv1 (vlax-curve-getfirstderiv p1 (vlax-curve-getparamatpoint p1 (vlax-curve-getclosestpointto p1 v))))
  (setq fv0 (HL:lengthV fv0 1.0))
  (setq fv1 (HL:lengthV fv1 1.0))
  (if (HL:eqp fv0 fv1);don't work
      nil
    (progn
      (setq outp (HL:outerproduct fv0 fv1))
      (= side (if (< outp 0)
          -1
          1
          )
      )  
    )
  )
)

(defun HL:createRorC (rorc v p0 p1 r / entsel0 entsel1)
  (setq entsel0 (list p0 (vlax-curve-getclosestpointto p0 v)))
  (setq entsel1 (list p1 (vlax-curve-getclosestpointto p1 v)))  
  (command-s rorc p0 p1)  
)

(defun c:filletPolyOneSide ( / r )
   (if (not HL:ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (HL:A_start)
  (setq r (HL:getInputR))
  (command-s "._fillet" "r" r)
  (HL:convexorconcaverorc r "._fillet")
  (HL:A_end)
)
(defun c:chamferPolyOneSide ( / c )
 (if (not HL:ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (HL:A_start)
  (setq c (HL:getInputR))
  (command-s "._chamfer" "d" c "" "")
  (HL:convexorconcaverorc c "._chamfer")
  (HL:A_end)
)
(defun HL:convexorconcaverorc ( r com / pline side v p0 p1 allelement ent tmpent createRlist entselP plineVertex pline vertex)
  
  ;input r  
  
  ;select pline  
  (setq plineVertex (HL:getPLineAndPoint))
  (setq pline (car plineVertex))
  (setq vertex (cdr plineVertex))      
  ;select witch side r get point
  (setq side (HL:getSide pline vertex))
  ;for all pline vertex
  (setq allelement nil)
  (setq tmpent (entlast))
  (foreach pair (HL:plineExplodeAndGetElementVertexAndPair pline)
    (setq v (car pair))
    (setq p0 (cadr pair))
    (setq p1 (cddr pair))
    (if (= allelement nil)
        (progn
          (setq allelement (cons p0 allelement))
          (setq allelement (cons p1 allelement))
        )
        (setq allelement (cons p1 allelement))
    )
    ;get outerproduct at vertex
    (if (HL:isSide v p0 p1 side)
        (progn
          (HL:createRorC com v p0 p1 r)
          (setq ent (entlast))
          (if (equal ent tmpent)
              (progn
                (prompt "can't create\n");cant create R                              
              )
              (progn
                (setq createRlist (cons ent createRlist))
                (setq tmpent ent)
              )
          )          
        )
    )   
  )
  
  (setq allelement (append allelement createRlist))
  (HL:joinAll allelement)  
)