;;; Scsh errno handling interface ---------------------------------------------
;; Part of scsh 0.7. See file COPYING for notices and license.
;; Implements errno-focused exception handling interface, using Scheme48 conditions
;; Extends work done by Olin Shivers

(define (with-errno-handler* handler thunk)
  (with-exception-handler
    (lambda (condition)
      (if (os-error? condition)
          (handler (os-error-code condition)
                   (list (condition-message condition)
                         (condition-who condition)
                         (condition-irritants condition)))
          (raise condition)))
    thunk))

(define (raise-errno-error errno/num who . irritants)
  (let ((errnum (cond ((errno? errno/num) (errno-os-number errno/num))
                     ((integer? errno/num) errno/num)
                     (else (error "Not an errno or integer" errno/num)))))
    (raise 
      (condition
        (make-os-error errnum)
        (make-who-condition who)
        (make-message-condition
          (os-string->string
          (byte-vector->os-string
            (os-error-message errnum))))
        (make-irritants-condition irritants)))))

; Legacy synonym
(define (errno-error errno/num who . irritants)
  (apply raise-errno-error errno/num who irritants))

(define-syntax weh-cond
  (syntax-rules (else)
    ((weh-cond () ((cond-condition cond-body) ...) error-number return)
     (cond (cond-condition cond-body) ...))
    ((weh-cond (((errno-name ...) clause-body ...) . other-clauses) (cond-clause ...) error-number return)
     (weh-cond other-clauses
               (cond-clause ... ((or (errno=? (errno errno-name) (integer->errno error-number)) ...)
                                 (call-with-values (lambda () clause-body ...) return)))
               error-number return))
    ((weh-cond ((else clause-body ...) . other-clauses) (cond-clause ...) error-number return)
     (weh-cond other-clauses
               (cond-clause ... (else (call-with-values (lambda () clause-body ...) return)))
               error-number return))))

(define-syntax with-errno-handler
  (syntax-rules ()
    ((with-errno-handler
      ((err data)
       (clause-condition clause-body ...) ...)
      body ...)
     (call-with-current-continuation
      (lambda (return)
        (with-errno-handler*
         (lambda (err data)
           (weh-cond ((clause-condition clause-body ...) ...) () err return))
         (lambda () body ...)))))))
