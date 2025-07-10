;;; User and Group DB Tests ----------------------------______-----------------
;; Part of scsh 1.0. See file COPYING for notices and license.
;; Extends work done by Christoph Hetz

;;; Test code -----------------------------------------------------------------
;; All procedures return either #t or #f

(add-test! 'user-info 'user-and-group-db-access
  (lambda ()
    (let* ((user-0 (user-info (getenv "USER")))
	   (user-name (user-info:name user-0))
	   (user-id (user-info:uid user-0))
	   (user-gid (user-info:gid user-0))
	   (user-hdir (user-info:home-dir user-0))
	   (user-shell (user-info:shell user-0))
	   (group-0 (group-info user-gid))
	   (group-name (group-info:name group-0))
	   (group-id (group-info:gid group-0))
	   (group-mem (group-info:members group-0)))
      (and (string? user-name)
	   (integer? user-id)
	   (integer? user-gid)
	   (string? user-hdir)
	   (string? user-shell)
	   (string? group-name)
	   (integer? group-id)
	   (list? group-mem)
	   (equal? user-name (user-info:name (user-info user-id)))
	   (equal? (user-info:name (user-info (getenv "USER"))) user-name)
	   (equal? group-id (group-info:gid (group-info group-name)))))))