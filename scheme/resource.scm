;;; Resources -----------------------------------------------------------------
;; Part of scsh 0.7. See file COPYING for notices and license.

(define-record-type :resource
  (make-resource align! lock)
  resource?
  (align! resource-align!)
  (lock resource-lock))


;; TODO: locks are considered outdated in scheme48, the preferred way to handle this is 
;; with optimistic concurrency. We should probably update the internal representation
;; to play nicer with scheme48
(define (with-resources-aligned resources thunk)
   (let ((locks (map resource-lock resources)))
     (apply obtain-all-or-none locks)
     (for-each
      (lambda (align!) (align!))
      (map resource-align! resources))
     (let ((val (with-handler
                 (lambda (cond more)
                   (for-each release-lock locks)
                   (more))
                 thunk)))
       (for-each release-lock locks)
       val)))
