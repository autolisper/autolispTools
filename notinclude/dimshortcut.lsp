(defun dimccur ()
  nil
)
(defun putArrow ()
  ;pick line  
  ;get vertexs
  ;investigate horozontal or vertical
  ;pick near vertex
  ;insert arrow block to the near vertex  
)
(defun putCL ()
  ;pick line  
  ;get vertexs
  ;investigate horozontal or vertical
  ;pick near vertex
  ;insert arrow block to the near vertex  
)
(defun docommand ( s )
  (cond
    ((= s "f") (command-s "._dimlinear"))
    ((= s "r") (command-s "._dimradius"))
    ((= s "d") (command-s "._dimdiameter"))
    ((= s "b") (command-s "._dimbaseline"))
    ((= s "a") (putArrow))
    ((= s "c") (dimccut))
  )
)
(defun c:dimshortcut( / $_device $_code $_data)  
  (setvar "CLAYER" "寸法線")
  (while (not (= $_data "q"))
    (setq $_device (grread T (+ 1 2 4 8) 0)
              $_code   (car $_device)
              $_data   (cadr $_device)
    )      
    (if (= $_code 2)
        (progn          
          (setq $_data (chr $_data))
          (docommand $_data)
        )
    )
  )  
  (princ)
)