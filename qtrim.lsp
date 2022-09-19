;;must load utility.lsp at first

(defun _qtrim ( l0 l1 lt / line l0s l1s lv bnormal op OldOsmode OldCmdEcho)  
  ;calc normal
  (setq lv (-L l1 l0))
  (setq bnormal (rotate90 lv))
  (setq bnormal (lengthV bnormal 0.1))
  ;if trimside is left side flip bnormal
  (setq op (outerproduct lv (-L lt l0)))
  (if (> op 0)
      (setq bnormal (list (- 0 (car bnormal)) (- 0 (cadr bnormal))))
  )
  ;calc trim line point
  (setq l0s (+L l0 bnormal))
  (setq l1s (+L l1 bnormal))
  (setq OldOsmode (getvar "OSMODE"))
  (setq OldCmdEcho (getvar "CMDECHO"))
	(setvar "OSMODE" 0)		
  (setvar "CMDECHO" 0)
  ;make line
  (command "._LINE" l0 l1 "")
  (setq line (entlast))
  ;trim
  (command "._trim" line "" "F" l0s l1s "" "")
  ;delete line
  (command "._erase" line "")
  (setvar "OSMODE" OldOsmode)
  (setvar "CMDECHO" OldCmdEcho)
)
(defun c:qtrim ( / l0 l1 lt isok)
  (if (not ISLOADUTILITY_LSP)
    (progn 
      (prompt "you must load utility.lsp\n")
      (quit)
    )
  )  
  (A_start)
  (setq isok T)  
  (while isok
    (setq l0 (getpoint "first point:"))
    (princ "\n")
    (if l0
      (progn
        (setq l1 (getpoint "second point:" l0))
        (princ "\n")
        (if (and l1 (not (eqp l0 l1)))
          (progn
            (setq lt (getpoint "pick one trim side point:"))
            (princ "\n")
            (if lt
              (_qtrim l0 l1 lt)
              (setq isok nil)
            )
          )
          (setq isok nil)
        )
      )
      (setq isok nil)
    )
  )
  (A_end)
)