;;; Copyright (c) 1994 by Olin Shivers
;;; Add scsh conditions to s48.

;;; A syscall-error condition-type:

(define-condition-type &syscall-error &error
  make-syscall-error syscall-error?
  (errno condition-errno)
  (syscall condition-syscall))

(define (errno-error errno syscall . irritants)
  (errno-error-with-message errno (errno-msg errno) syscall irritants))

(define (errno-error-with-message errno msg syscall . irritants)
  (raise
   (condition
    (make-syscall-error errno syscall)
    (make-message-condition msg)
    (make-irritants-condition irritants))))

(define (with-errno-handler* handler thunk)
  (with-handler
    (lambda (condition more)
      (cond ((syscall-error? condition)
             (handler (condition-errno condition)
                      (list (condition-message condition)
                            (condition-syscall condition)
                            (condition-irritants condition))))
            ((os-error? condition)
             (handler (os-error-code condition)
                      (list (condition-message condition)
                            (condition-who condtion)
                            (condition-irritants condition))))
            (else (more))))
    thunk))

;;; (with-errno-handler
;;;   ((errno data) ; These are vars bound in this scope.
;;;    ((errno/exist) . body1)
;;;    ((errno/wouldblock errno/again) . body2)
;;;    (else . body3))
;;;
;;;   . body)

(define-syntax with-errno-handler
  (lambda (exp rename compare)
    (let* ((%lambda (rename 'lambda))
           (%cond (rename 'cond))
           (%else (rename 'else))
           (%weh (rename 'with-errno-handler*))
           (%= (rename '=))
           (%begin (rename `begin))
           (%or (rename `or))
           (%call/cc (rename 'call-with-current-continuation))
           (%cwv (rename 'call-with-values))

           (%ret (rename 'ret)) ; I think this is the way to gensym.

           (err-var (caaadr exp))
           (data-var (car (cdaadr exp)))
           (clauses (cdadr exp))
           (body (cddr exp))

           (arms (map (lambda (clause)
                        (let ((test (if (compare (car clause) %else)
                                        %else
                                        (let ((errs (car clause)))
                                          `(,%or . ,(map (lambda (err)
                                                           `(,%= ,err ,err-var))
                                                         errs))))))
                          `(,test
                            (,%cwv (,%lambda () . ,(cdr clause)) ,%ret))))
                      clauses)))

      `(,%call/cc (,%lambda (,%ret)
         (,%weh
            (,%lambda (,err-var ,data-var)
              (,%cond . ,arms))
            (,%lambda () . ,body)))))))
