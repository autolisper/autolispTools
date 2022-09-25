(defun LM:flatten ( l )
    (if (atom l)
        (list l)
        (append (LM:flatten (car l)) (if (cdr l) (LM:flatten (cdr l))))
    )
)

(defun modifywipeout (w nextpoints / o u v nu nv lu lv center localpoints ent)
  (setq nextpoints (append nextpoints (list (car nextpoints))))
  (setq o (A_dxf 10 w))  
  (setq u (A_dxf 11 w))
  (setq v (A_dxf 12 w))  
  (setq nu (lengthV u 1.0 ))
  (setq nv (lengthV v 1.0 ))
  (setq lu (getL u))
  (setq lv (getL v))  
  (setq center (+L o (+L (scalarMul 0.5 u) (scalarMul 0.5 v))))  
  (defun toUV (p)
    (setq p (-L p center))
    (list (/ (innerproduct nu p) lu) (neg (/ (innerproduct nv p) lv)) 0.0)
  )  
  (setq localpoints (mapcar 'toUV nextpoints))
  (setq ent (vl-remove-if '(lambda (e) (= (car e) 14)) (entget w)))    
  (setq ent (append ent (mapcar '(lambda (p) (cons 14 p)) localpoints)))    
  (entmod ent)    
)
(defun makewipeout ( points / minx miny maxx maxy origin lmax center u v hlmax uvs ent)
  ;get origin
  (setq minx (apply 'min (mapcar 'car points)))
  (setq maxx (apply 'max (mapcar 'car points)))
  (setq miny (apply 'min (mapcar 'cadr points)))
  (setq maxy (apply 'max (mapcar 'cadr points)))
  (setq lmax (max (- maxx minx) (- maxy miny)))
  (setq hlmax (/ lmax 2.0))
  (setq origin (list minx miny 0.0))
  ;getU
  (setq u (list lmax 0.0 0.0))
  ;getV
  (setq v (list 0.0 lmax 0.0))  
  ;calc center
  (setq center (+L origin (list hlmax hlmax 0.0)))
  (defun toUV (p)
    (setq p (-L p center))
    (list 14 (/ (car p) lmax) (neg (/ (cadr p) lmax)) 0.0)
  )
  (setq uvs (mapcar 'toUV points))
  ;;to cycle
  (setq uvs (append uvs (list (car uvs))))
  (setq ent (append 
              '((0 . "WIPEOUT")(100 . "AcDbEntity") (100 . "AcDbWipeout"))
               (list (cons 10 origin) (cons 11 u) (cons 12 v))
               '((70 . 7) (280 . 1) (281 . 50) (282 . 50) (283 . 0) (290 . 0) (71 . 2))
                uvs
              )              
  )                
  (entmake ent)
)
(defun getCornerPoint (point0 point1 / maxx minx miny maxy)
  (setq minx (min (car point0) (car point1)))
  (setq maxx (max (car point0) (car point1)))
  (setq miny (min (cadr point0) (cadr point1)))
  (setq maxy (max (cadr point0) (cadr point1)))
  (list (list minx miny 0.0) (list maxx miny 0.0) (list maxx maxy 0.0) (list minx maxy 0.0))
)
(defun getClosedLines ( corners / fp sp len l lines i)  
  (setq len (length corners))
  (setq i 0)
  (repeat len
    (setq fp (nth i corners))
    (setq sp (nth (rem (1+ i) len) corners))
    (setq fp (vlax-3d-point fp))
    (setq sp (vlax-3d-point sp))
    (setq l (vla-addline (modelspace) fp sp))
    (setq lines (cons l lines))
    (setq i (1+ i))
  )
  (setq lines (reverse lines))    
)
(defun getRectLines (point0 point1 / corners)
  (setq corners (getCornerPoint point0 point1))    
  (getClosedLines corners)
)
(defun getPlayerTriangle ( st l ang hang / )
  (list st (polar st (+ ang hang) l) (polar st (- ang hang) l) )
)
(defun getIntersect (l p0 p1)
  (setq l (vlax-vla-object->ename l))
  (inters (vlax-curve-getstartpoint l) (vlax-curve-getendpoint l) p0 p1)
)
(defun getWipeoutlist ( t0 l ang hang lines corners / i playert intline st wipeoutlist)
  (setq playert (getPlayerTriangle t0 l ang hang))
  ;get intersect point
  (setq i 0)
  (setq intline nil)
  (repeat 4
    (if (not intline)
      (progn        
        (setq st (getIntersect (nth i lines) (car playert) (polar (car playert) (+ ang pi) (* l 10000))))        
        (if st
            (progn
              (setq intline i)            
            )
        )
      )
    )
    (setq i (1+ i))
  )  
  (setq wipeoutlist (list st (car playert) (cadr playert) (caddr playert) (car playert) st 
        (nth (rem (+ intline 1) 4) corners)
        (nth (rem (+ intline 2) 4) corners) 
        (nth (rem (+ intline 3) 4) corners)
        (nth (rem (+ intline 4) 4) corners)
  ))  
)
(defun setIn (point minx maxx miny maxy eps / pointx pointy)
  (setq pointx (car point))
  (setq pointy (cadr point))
  (setq minx (+ minx eps))
  (setq miny (+ miny eps))
  (setq maxx (- maxx eps))
  (setq maxy (- maxy eps))
  (setq pointx (max minx pointx))
  (setq pointy (max miny pointy))
  (setq pointx (min maxx pointx))
  (setq pointy (min maxy pointy))
  (list pointx pointy 0.0)
)
(defun makeButton (point size text / minx miny maxx maxy textsize textobject lines textpoint)
  (setq minx (car point))
  (setq miny (cadr point))
  (setq maxx (+ minx (car size)))
  (setq maxy (+ miny (cadr size)))
  (setq textsize (min (car size) (cadr size)))  
  (setq textpoint (+L point (scalarMul 0.5 size)))
  (setq textobject (vla-addtext (modelspace) text (vlax-3d-point textpoint) textsize))  
  (vla-put-alignment textobject acAlignmentMiddle)
  (vla-put-TextAlignmentPoint textobject (vlax-3d-point textpoint))
  (setq lines (getRectLines point (+L point size)))
  (list (list minx maxx miny maxy) (cons textobject lines))  
)
(defun addTextCenter (point size text / textobject)
  (setq textobject (vla-addtext (modelspace) text (vlax-3d-point point) size))  
  (vla-put-alignment textobject acAlignmentMiddle)
  (vla-put-TextAlignmentPoint textobject (vlax-3d-point point))
  textobject
)
(defun isInButton (point button / minx maxx miny maxy x y plist) 
  (setq plist (car button))
  (setq minx (car plist))
  (setq maxx (cadr plist))
  (setq miny (caddr plist))
  (setq maxy (cadddr plist))  
  (setq x (car point))
  (setq y (cadr point))
  (and (<= minx x) (<= x maxx) (<= miny y) (<= y maxy))
)
(defun deleteButton ( button )
  (mapcar 'vla-delete (cadr button))
)
(defun c:whatisitgame ( / oldsvalue point0 point1 ang hang st t0 t1 t2 l minx maxx miny maxy corners lines playert playerl i answertext intline intp wipeoutlist wipeout WF plines text tpoint abutton isok buttonSize buttonLeftDown clearobject gbutton endtext hbutton helptext pre timepassed $_code sat0 $_data dav av dv v $_device)  
  (if (not ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (A_start)
  (setq oldsvalue (getvar "SELECTIONPREVIEW"))
  (setvar "SELECTIONPREVIEW" 0)    
  (randinit)  
  (setq ang 0.8)
  (setq hang 0.4)
  (while (or (not point0) (not point1) (eqp point0 point1))
  ;get point0
    (setq point0 nil)
    (setq point1 nil)
    (while (not point0)
      (setq point0 (getpoint "field corner first point:"))
      (princ "\n")
    )    
  ;get point1  
    (setq point1 (getcorner point0 "field corner second point:"))
    (princ "\n")
  )
  (setq minx (min (car point0) (car point1)))
  (setq maxx (max (car point0) (car point1)))
  (setq miny (min (cadr point0) (cadr point1)))
  (setq maxy (max (cadr point0) (cadr point1)))  
  (setq l (min (- maxx minx) (- maxy miny)))    
  (setq maxx (+ minx l))
  (setq maxy (+ miny l))
  (setq point0 (list minx miny 0.0))
  (setq point1 (list maxx maxy 0.0))
  (setq playerl (/ l 6.0))
  ;(setq t0 (scalarMul 0.5 (+L point0 point1)))
  (setq t0 (+L point0 (list (/ l 10.0) (/ l 10.0) 0.0)))
  (setq tpoint (list (+ minx (* l 0.15)) (+ miny (* l 0.15)) 0.0))  
  (setq answertext (chr (randrange 33 127)))
  (setq text (addTextCenter (scalarMul 0.5 (+L point0 point1)) (* l 0.8) answertext));)vla-addtext (modelspace)  answertext (vlax-3d-point tpoint) (* l 0.7)))  
  (setq corners (getCornerPoint point0 point1))
  (setq lines (getClosedLines corners))   
  (setq plines (getClosedLines (getPlayerTriangle t0 playerl ang hang)))
  (setq wipeoutlist (getWipeoutlist  t0 playerl ang hang lines corners))
  (setq buttonSize (list (/ l 2.0) (/ l 13.0) 0.0))  
  (setq buttonLeftDown (list (+ maxx (/ l 13.0)) miny 0.0))  
  (setq abutton (makeButton buttonLeftDown buttonSize "answer"))
  (setq gbutton (makeButton (+L (list 0.0 (* 3 (cadr buttonSize)) 0.0) buttonLeftDown) buttonSize "give up"))
  (setq hbutton (makeButton (+L (list 0.0 (* 6 (cadr buttonSize)) 0.0) buttonLeftDown) buttonSize "help"))
  (setq helptext "answer a letter behind wipeout!!\nmove->w a s d. \nanswer-> click the answer button and type \ngive up -> click the give up button")
  (makewipeout wipeoutlist)
  (setq wipeout (entlast))   
  (setq WF (getvar "WIPEOUTFRAME"))
  (setvar "WIPEOUTFRAME" 0)
  (setq pre (getvar "MILLISECS"))
  (setq v (/ l 10.0)) 
  (setq av (/ pi 10))
  (setq isok nil)
  (vla-zoomwindow (acadobject) (vlax-3d-point point0) (vlax-3d-point (list (+ (car buttonSize) (car point1)) (cadr point1) 0.0)) )
  (while (not isok)
    (setq $_device (grread T (+ 1 2 4 8) 0)
              $_code   (car $_device)
              $_data   (cadr $_device)
    )    
    (if (= $_code 3)                   ;Mouse Move      
        (progn
          (if (isInButton $_data abutton)                        
              (setq isok (if (= (getstring) answertext) 1))            
          )
          (if (isInButton $_data gbutton)                        
              (setq isok 2)
          )
          (if (isInButton $_data hbutton)                        
              (alert helptext)
          )
        )
    )    
    ( if (= $_code 2)
      (progn                
        (if (member $_data '(119 97 100 115))     
            (progn              
              (setq timepassed 0.5)
              (setq dv (* v timepassed))
              (setq dav (* av timepassed))
              (setq sat0 (polar '(0.0 0.0 0.0) ang dv))
              (cond 
                ((= $_data 119) (setq t0 (+L t0 sat0)))
                ((= $_data 115) (setq t0 (-L t0 sat0)))
                ((= $_data 97) (setq ang (+ ang dav)))
                ((= $_data 100) (setq ang (- ang dav)))
              )
              (setq t0 (setIn t0 minx maxx miny maxy (/ playerl 10)))
              (setq wipeoutlist (getWipeoutlist t0 playerl ang hang lines corners))
              (mapcar 'vla-delete plines)
              (setq plines (getClosedLines (getPlayerTriangle t0 playerl ang hang)))        
              (modifywipeout wipeout wipeoutlist)
              (command-s "._draworder" (vlax-vla-object->ename text) "" "B")
            )            
        )
      )    
    )  
  )
  (setvar "WIPEOUTFRAME" WF)
  ;delete wipeout
  (vla-delete (vlax-ename->vla-object wipeout))
  (setq endtext (if (= isok 1) "clear!! click to end" "give up!! click to end"))  
  (setq clearobject (addTextCenter (scalarMul 0.5 (+L point0 point1)) (/ l 10)  endtext))
  (vla-put-color clearobject 1)
  (command-s "._draworder" (vlax-vla-object->ename clearobject) "" "F")  
  ;delete player line
  (mapcar 'vla-delete plines)
  (getpoint)
  (vla-delete clearobject)  
  ;delete outer line
  (mapcar 'vla-delete lines)
  
  ;delete text
  (vla-delete text)
  ;delete button
  (deleteButton abutton)  
  (deleteButton gbutton)  
  (deleteButton hbutton)  
  (setvar "SELECTIONPREVIEW" oldsvalue)
  (A_end)
)