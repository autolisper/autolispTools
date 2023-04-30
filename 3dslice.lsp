
(defun c:3dslice ( / object point0 point1 point2 leftvec downvec slicepitch regions iscutting notcuttingnumber solidObjs height taperAngle oldOsmode preent ent nextpreent copyregion)
  (setq object (vlax-ename->vla-object (car (entsel "select 3d object:"))))
  (setq point0 (getpoint "select base point:"))
  (princ)
  (setq slicepitch (getreal "slice pitch:"))
  (setq leftvec (list 1 0 0))
  (setq downvec (list 0 1 0))    
  ;loop while vla-SelectionSolid return not null
  (setq notcuttingnumber 0)
  (while (or (and (not iscutting) (< notcuttingnumber 10)) region)  
    
    (setq point1 (HL:+L point0 leftvec))
    (setq point2 (HL:+L point0 downvec))
    ;vla-SelectionSolid return null if no intersection
    (setq region (vla-SectionSolid object (vlax-3d-point point0) (vlax-3d-point point1) (vlax-3d-point point2)))  
    (if iscutting
      (progn
        (if region
          (progn
            (setq regions (cons region regions))
          )
        )
      )
      (progn
        (setq notcuttingnumber (+ notcuttingnumber 1))
        (if region
          (progn
            (setq iscutting T)
            (setq regions (cons region regions))            
          )
        )
      )
    )
    (setq point0 (HL:+L point0 (list 0 0 slicepitch)))
  )  
  ;convert regions to extrude solid
  (setq oldOsmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq height slicepitch)
  (setq taperAngle 0.0)
  ;(setq preent (entlast))
  (foreach region regions
    (setq copyregion (vla-copy region))
    (command-s "._extrude" (vlax-vla-object->ename region) "" "d" (list 0 0 0) (list 0 0 height)) 
    (command-s "._extrude" (vlax-vla-object->ename copyregion) "" "d" (list 0 0 0) (list 0 0 (HL:neg slicepitch)))         
    (setq height (+ height slicepitch))    
    ;seperate region can't extrude by vla-AddExtrudedSolid
    ;(setq solidObjs (cons (vla-AddExtrudedSolid (HL:modelSpace) region height taperAngle) solidObjs))
  )
  ;(command-s "._union" (HL:toSet solidObjs) "")
  (setvar "OSMODE" oldOsmode)
)