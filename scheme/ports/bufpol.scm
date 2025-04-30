;; Enum, represeting possile buffering policies
(define-enumerated-type buf-policy :buf-policy
  buf-policy?				; predicate
  buf-policies          ; vector containing all elements
  buf-policy-name			; name accessor
  buf-policy-index			; index accessor
  ( 
    block   ; General block buffering (general default)
    line    ; Line buffering (tty default) 
    none    ; Direct I/O -- no buffering
  ))

(define bufpol/block (buf-policy block))
(define bufpol/line (buf-policy line))
(define bufpol/none (buf-policy none))

(define (buf-policy=? p1 p2)
  (= (buf-policy-index p1) (buf-policy-index p2)))