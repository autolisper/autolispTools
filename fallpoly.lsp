(defun HL:polyintersect (o0 o1 / intPoints tempPoint)
  (setq intPoints (vla-IntersectWith o0 o1 acExtendNone))           
  (not (vl-catch-all-error-p
            (setq tempPoint (vl-catch-all-apply
            'vlax-safearray->list (list (vlax-variant-value intPoints))))))         
)
(setq HL:BINARYSEARCHEPS 0.0000001)
;0.001 is the smallest distance that can be moved
;if intersect return nil, else return t
(defun HL:binarymove (o0 o1 vec / nextpos movel)
  (setq o0 (vlax-ename->vla-object o0))
  (setq o1 (vlax-ename->vla-object o1))
  (setq nowpos (list 0.0 0.0 0.0))
  (setq nextpos (HL:+L nowpos vec))
  (setq movel (HL:getL vec))
  (setq vec (HL:lengthV vec 1.0))
  ;move o0 nowpos to nextpos  
  (vla-Move o0 (vlax-3d-point nowpos) (vlax-3d-point nextpos))
  (if (HL:polyintersect o0 o1)  
    (progn
      (vla-Move o0 (vlax-3d-point nextpos) (vlax-3d-point nowpos))
      (while (> movel HL:BINARYSEARCHEPS)
        (setq movel (/ movel 2.0))
        (setq nextpos (HL:+L nowpos (HL:scalarMul movel vec)))
        (vla-Move o0 (vlax-3d-point nowpos) (vlax-3d-point nextpos))
        (if (HL:polyintersect o0 o1)
          ;if o0 and o1 intersect, then move o0 to nowpos
          (progn
            (vla-Move o0 (vlax-3d-point nextpos) (vlax-3d-point nowpos))        
          )          
          (setq nowpos nextpos)
        )
      )
      nil
    )
    t
  )
)
(defun HL:moveToObject (o0 o1 vec / nowpos)
  (while (HL:binarymove o0 o1 dvec))  
)