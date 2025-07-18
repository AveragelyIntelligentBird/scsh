;;; Process State Tests ---------------------------------------------------------
;; Part of scsh 1.0. See file COPYING for notices and license.
;; Extends work done by David Frese

;;; Test code -----------------------------------------------------------------
;; All procedures return either #t or #f

;; --- umask stuff ---

(add-test! 'with-umask 'process-state
	   (lambda (new-umask)
	     (let ((old-umask (umask)))
	       (and (with-umask new-umask
                (file-mode=? (umask) new-umask))
              (file-mode=? (umask) old-umask))))
	   (integer->file-mode 0))

(add-test! 'set-umask 'process-state
	   (lambda (new-umask)
	     (let ((old-umask (umask)))
	       (set-umask new-umask)
	       (let ((res (umask)))
		 (set-umask old-umask)
		 (file-mode=? res new-umask))))
     (file-mode other))

;; --- cwd stuff ---

(add-test! 'with-cwd 'process-state
	   (lambda (new-cwd)
	     (let ((old-cwd (cwd)))
	       (and
		(with-cwd new-cwd
			  (equal? (cwd) new-cwd))
		(equal? (cwd) old-cwd))))
	   "/")

(add-test! 'chdir 'process-state
	   (lambda (new-cwd)
	     (let ((old-cwd (cwd)))
	       (chdir new-cwd)
	       (let ((res (cwd)))
		 (chdir old-cwd)
		 (equal? res new-cwd))))
	   "/")

