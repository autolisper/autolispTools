;;must load utility.lsp at firsteright90

(defun HL:_qtrim ( l0 l1 lt / line l0s l1s lv bnormal op OldOsmode OldCmdEcho)  
  ;calc normal
  (setq lv (HL:-L l1 l0))
  (setq bnormal (HL:rotateright90 lv))
  (setq bnormal (HL:lengthV bnormal 0.1))
  ;if trimside is left side flip bnormal
  (setq op (HL:outerproduct lv (HL:-L lt l0)))
  (if (> op 0)
      (setq bnormal (list (- 0 (car bnormal)) (- 0 (cadr bnormal))))
  )
  ;calc trim line point
  (setq l0s (HL:+L l0 bnormal))
  (setq l1s (HL:+L l1 bnormal))
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
  (if (not HL:ISLOADUTILITY_LSP)
    (progn 
      (alert "you must load utility.lsp\n")
      (quit)
    )
  )  
  (HL:A_start)
  (setq isok T)  
  (while isok
    (setq l0 (getpoint "first point:"))
    (princ "\n")
    (if l0
      (progn
        (setq l1 (getpoint "second point:" l0))
        (princ "\n")
        (if (and l1 (not (HL:eqp l0 l1)))
          (progn
            (setq lt (getpoint "pick one trim side point:"))
            (princ "\n")
            (if lt
              (HL:_qtrim l0 l1 lt)
              (setq isok nil)
            )
          )
          (setq isok nil)
        )
      )
      (setq isok nil)
    )
  )
  (HL:A_end)
)