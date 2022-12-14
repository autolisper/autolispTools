(defun c:clearallcirclegame ( / point0 point1 left down size right up oldosmode fieldlines l rsize isok $device $code $data ball vx vy ballcenter boundcircles ccenters crs vlength ang nowr prepoint movedistance)  
  (if (not HL:ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (HL:A_start)
  (HL:randinit)    
  ;
  
  (while (or (not point0) (not point1) (HL:eqp point0 point1))
  ;get point0
    (setq point0 nil)
    (setq point1 nil)
    (while (not point0)
      (setq point0 (HL:getpointW nil "field corner first point:"))
      (princ "\n")
    )    
  ;get point1  
    (setq point1 (HL:getcornerW point0 "field corner second point:"))
    (princ "\n")
  )  
  ;get size  
  (setq left (min (car point0) (car point1)))
  (setq down (min (cadr point0) (cadr point1)))
  (setq size (min (abs (- (car point0) (car point1))) (abs (- (cadr point0) (cadr point1)))))  
  (setq right (+ left size))
  (setq up (+ down size))    
  (setq rsize (* size 0.035))
  (setq nowr rsize)
  (setq vlength (/ rsize 2.0))
  (setq ang (HL:randreal (* 2.0 pi)))
  (setq vlength (polar (list 0 0) ang vlength))
  (setq vx (car vlength))
  (setq vy (cadr vlength))
  ;draw field
  (setq oldosmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)  
  (setq fieldlines (cons (HL:drawLine (list left down) (list right down)) fieldlines))
  (setq fieldlines (cons (HL:drawLine (list right down) (list right up)) fieldlines))
  (setq fieldlines (cons (HL:drawLine (list right up) (list left up)) fieldlines))
  (setq fieldlines (cons (HL:drawLine (list left up) (list left down)) fieldlines))
  (vla-zoomwindow (HL:acadobject) (vlax-3d-point (list (- left (/ size 5)) (- down (/ size 5)))) (vlax-3d-point (list (+ right (/ size 5)) (+ up (/ size 5) ) )))
  (setq ballcenter (list (+ left (* rsize 1.25)) (/ (+ up down) 2.0)))
  (setq ball (HL:drawCircle ballcenter rsize))   
  (vla-put-color ball 3)
  (defun HL:getModifyCenterBound (ballcenter bcenter br / p dv)
    (setq p (HL:-L ballcenter bcenter))    
    (HL:+L bcenter (HL:lengthV p (+ nowr br)))   
  )
  (defun HL:isCollid (p0 c0 p1 c1)
    (<= (HL:getD p0 p1) (+ c0 c1))
  )
  (defun HL:getBoundV (p0 p1 vx vy / p v normalp nv hv)
    (setq p (HL:-L p1 p0))
    (setq v (list vx vy))
    (setq normalp (HL:lengthV p 1.0))
    (setq nv (HL:scalarMul (HL:innerproduct normalp v) normalp ))
    (setq hv (HL:-L v nv))
    (HL:-L hv nv)    
  )  
  (defun HL:modifyballcenterAndV ( movedistance / bl bu br bd cx cy i tcenter tr ncs ncc ncr v nextcenter center tmppoint tmpr endtext clearobject)
    ;;collidcenter
    (setq center ballcenter)
    (setq i 0)    
    (while (< i (length boundcircles))
      (setq tcenter (nth i ccenters ))
      (setq tr (nth i crs ))
      (if (HL:isCollid center nowr tcenter tr)
          (progn
            (setq v (HL:getBoundV center tcenter vx vy))
            (setq vx (car v))
            (setq vy (cadr v))
            (setq center (HL:getModifyCenterBound center tcenter tr))
            (if (= (vla-get-color (nth i boundcircles)) 6)
              (setq nowr rsize)
            )
            (vla-delete (nth i boundcircles))                        
          )
          (progn
            (setq ncs (cons (nth i boundcircles) ncs))
            (setq ncc (cons tcenter ncc))
            (setq ncr (cons tr ncr))
          )
      )
      (setq i (1+ i))
    )
    (setq boundcircles ncs)
    (setq ccenters ncc)
    (setq crs ncr)
    ;modify center
    (setq ballcenter center)

    (setq cx (car center))
    (setq cy (cadr center))
    (setq bl (- cx nowr))
    (setq br (+ cx nowr))
    (setq bd (- cy nowr))
    (setq bu (+ cy nowr))    
    (if (<= bl left)
        (setq cx (+ left (* nowr 1.00001)))
    )
    (if (<= right br)
        (setq cx (- right (* nowr 1.00001)))
    )
    (if (<= bd down)
        (setq cy (+ down (* nowr 1.00001)))
    )
    (if (<= up bu)        
        (setq cy (- up (* nowr 1.00001)))      
    )
    (setq nextcenter (list cx cy))
    (if (not (= (car nextcenter) (car ballcenter)))
        (setq vx (HL:neg vx))       
    )
    (if (not (= (cadr nextcenter) (cadr ballcenter)))
        (setq vy (HL:neg vy))       
    )
    (setq ballcenter nextcenter)
    (vla-put-center ball (vlax-3d-point ballcenter))
    (setq nowr (- nowr (* 20.0 movedistance (/ rsize 300.0))))
    (if (< 0 nowr)
      (vla-put-radius ball nowr)
    )
    (vla-update ball)    
  )
  
  (defun HL:movecircle ( movedistance ) 
    (setq ballcenter (HL:+L ballcenter (HL:scalarMul (* 10.0 movedistance) (list vx vy))))
    (HL:modifyballcenterAndV movedistance)    
  )  
  (defun HL:addboundcircle (point color / tmpr c)
    (setq tmpr rsize);(HL:getD tmppoint $_data))
    (if (not (HL:isCollid ballcenter nowr point tmpr))          
      (progn
      (setq c (HL:drawCircle point tmpr))
      (vla-put-color c color)
      (setq boundcircles (cons c boundcircles))
      (setq ccenters (cons point ccenters))
      (setq crs (cons tmpr crs))
      )                          
    )                             
  )
  (repeat 10
    (HL:addboundcircle (list (HL:randrealrange (+ left rsize) (- right rsize)) (HL:randrealrange (+ down rsize) (- up rsize))) 6)
  )
  (while (and (< 0 (length boundcircles)) (< 0 nowr))
    (setq $_device (grread T (+ 1 2 4 8) 0)
              $_code   (car $_device)
              $_data   (cadr $_device)
    )    
    (cond ((= $_code 3)                   ;Mouse Click      
        (progn          
              ; (setq tmpr rsize);(HL:getD tmppoint $_data))
              ; (if (not (HL:isCollid ballcenter rsize $_data tmpr))          
              ;   (progn
              ;     (setq c (HL:drawCircle $_data tmpr))
              ;     (setq boundcircles (cons c boundcircles))
              ;     (setq ccenters (cons $_data ccenters))
              ;     (setq crs (cons tmpr crs))
              ;   )                          
              ; )                             
          (HL:addboundcircle $_data 7)
        )
        )
        ((= $_code 5)
          (if prepoint
            (progn
              (setq movedistance (/ (HL:getD prepoint $_data) size))
              (HL:movecircle movedistance)
            )
          )
          (setq prepoint $_data)
        )  
        ((= $_code 2)                     
          (cond 
                ((= $_data 113)
                  (setq isok T)
                )
          )          
        )
    )
    
  )
  (setq endtext (if (= (length boundcircles) 0) "clear!! click to end" "game over!! click to end"))  
  (setq clearobject (HL:addTextCenter (HL:scalarMul 0.5 (HL:+L (list left down) (list right up))) (/ size 10)  endtext))
  (vla-put-color clearobject 1)
  (command-s "._draworder" (vlax-vla-object->ename clearobject) "" "F")  
  (getpoint)
  (foreach l fieldlines
    (vla-delete l)
  )
  (foreach l boundcircles
    (vla-delete l)
  )
  (vla-delete ball)
  (vla-delete clearobject)
  (setvar "OSMODE" oldosmode)
  (HL:A_end)
)