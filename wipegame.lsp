
;;must load utility.lsp at first

(defun createarray (i j / ret ii jj)
  (setq ret nil)
  (setq ii 0)  
  (while (< ii i )
    (setq jj 0)
    (while (< jj j)      
      (setq ret (cons (cons (cons ii jj) 0) ret))
      (setq jj (1+ jj))
    )    
    (setq ii (1+ ii))
  )
  ret
)
(defun getAtV (array i j)
  (cdr (assoc (cons i j) array))
)
(defun updateAt (array i j u)
  (subst (cons (cons i j) u) (assoc (cons i j) array) array)
)
(defun c:wipegame ( / i j x0 y0 x1 y1 point0 point1 size gamearray point left down right up sizei sizej num fieldlines cellsize rsize circles l) 
  (if (not ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (A_start)
  (randinit)  
  (setq num 0)  
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
  (while (not (and (<= 2 num) (<= num 10)))
    (setq num (getint "field size(2~10):"))
    (princ "\n")
  )
  ;get size  
  (setq left (min (car point0) (car point1)))
  (setq down (min (cadr point0) (cadr point1)))
  (setq size (min (abs (- (car point0) (car point1))) (abs (- (cadr point0) (cadr point1)))))  
  (setq right (+ left size))
  (setq up (+ down size))
  (setq cellsize (/ size (float num)))
  (setq rsize (/ cellsize 2.0))
  ;draw field
  (setvar "OSMODE" 0)
  (setq i 0)
  (while (<= i num)
    (setq x0 (+ left (* i cellsize)))
    (setq x1 x0)
    (setq y0 down)
    (setq y1 up)
    (command-s "._LINE" (list x0 y0) (list x1 y1) "")
    (setq fieldlines (cons (entlast) fieldlines))
    (setq i (1+ i))
  )
  (setq i 0)
  (while (<= i num)
    (setq x0 left)
    (setq x1 right)
    (setq y0 (+ down (* i cellsize)))
    (setq y1 y0)
    (command-s "._LINE" (list x0 y0) (list x1 y1) "")
    (setq fieldlines (cons (entlast) fieldlines))
    (setq i (1+ i))
  )
  (command "._zoom" "o" (toSet fieldlines) "")
  
  ; point -> i,j
  (defun getIJ ( p / tx ty)  
    (setq tx (/ (- (car p) left) cellsize))
    (setq ty (/ (- (cadr p) down) cellsize))
    (if (or (< tx 0) (< ty 0) (<= num tx) (<= num ty))
        nil
        (list (fix tx) (fix ty))
    )
  )  
  (defun getCenter (tx ty)
    (list (+ left (* tx cellsize) rsize) (+ down (* ty cellsize) rsize)) 
  )
  (defun updatearray ( ii jj / c v)      
      (if (and (<= 0 ii) (<= 0 jj) (< ii num) (< jj num))
        (progn
          (setq v (getAtV gamearray ii jj))          
          (if (numberp v)
              (progn
                (setvar "OSMODE" 0)
                (command-s "._CIRCLE" (getCenter ii jj) rsize)
                (setq circles (cons (entlast) circles))
                (setq gamearray (updateAt gamearray ii jj (entlast)))
              )
              (progn            
                (entdel v)
                (setq circles (vl-remove v circles))
                (setq gamearray (updateAt gamearray ii jj 0))
              )
          )
        )
      )
  )
  (defun isgameEnd ()
    (vl-every 'numberp (mapcar 'cdr gamearray))
  )
  (defun updatearray5 (i j)
    (updatearray i j)
    (updatearray (1- i) j)
    (updatearray (1+ i) j)
    (updatearray i (1+ j))
    (updatearray i (1- j))
  )
  ;random initialize
  (setq gamearray (createarray num num))  
  (while (isgameEnd)
    (setq i 0)  
    (while (< i (* 5 num))
      (updatearray5 (rand num) (rand num))
      (setq i (1+ i))
    )
  )
  
  ;game loop

  (while (not (isgameEnd))
    ;pick one point
    (setq point nil)
    (while (not point)
      (setq point (getpoint))
    )    
    (setq point (getIJ point))
    (if point
      (progn        
        (setq i (car point))
        (setq j (cadr point))
        (updatearray5 i j)    
      )
    )
  )
  ;draw clear
  (command "._text" (list left (+ down ( / size 2))) (/ (* rsize num) 5) 0 "clear!! click to end.")
  ;click to end
  (getpoint)
  (entdel (entlast))
  ;clear game
  (foreach l fieldlines
    (entdel l)
  )
  (foreach l circles
    (entdel l)
  )
  (A_end)
  (princ)
)