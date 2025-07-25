(define-interface test-base-interface
  (export add-test!
	  add-test-multiple!
	  test-all
	  test-group
	  test-single
	  test-single/args
	  test-summary))

(define-structure test-base test-base-interface
  (open scsh-user
        handle
        (subset srfi-1 (filter
                        delete!
                        find
                        fold-right))
	define-record-types)
  (files test-base))

;;; System Calls --------------------------------------------------------------

;; Errors

;; I/O

;; File System
(define-structure file-system-test (export)
  (open scsh-user
        (subset posix-files (file-options file-mode file-mode=? file-mode-
                             integer->file-mode file-mode->integer))
        (subset scsh-fdport-internal (fdport->fd))
        test-base)
  (files file-system-tests))

;; Processes

;; Process state
(define-structure process-state-test (export)
  (open	scsh-user
        test-base
        (subset posix-files (file-mode file-mode=? integer->file-mode)))
  (files process-state-tests))

;; User and group database access
(define-structure user-and-group-db-access-test (export)
  (open scsh-user
        test-base)
  (files user-and-group-db-access))

;; Accessing command-line aguments

;; System parameters
(define-structure system-parameter-test (export)
  (open scsh-user
	test-base)
  (begin
    (add-test! 'uname-direct 'system-parameters
	       (lambda ()
		 (and (> (string-length (uname-os)) 0)
          (> (string-length (uname-node)) 0)
          (> (string-length (uname-release)) 0)
          (> (string-length (uname-version)) 0)
          (> (string-length (uname-machine)) 0))))

    (add-test! 'uname 'system-parameters
	       (lambda ()
		 (let ((uname-rec (uname)))
		   (> (string-length (uname:node-name uname-rec)) 0))))

    (add-test! 'system-name 'system-parameters
	       (lambda ()
		 (> (string-length (system-name)) 0)))))

;; Signal system

;; Time
(define-structure datetime-test (export)
  (open scsh-user
        test-base)
  (files datetime-tests))

;; Environment variables
(define-structure envvar-test
  (export)
  (open scsh-user
	      test-base
        debug-messages
        thread-fluids
        (subset srfi-1 (every)))
  (files envvar-test))

;; Terminal device control
(define-structure terminal-device-control-test (export)
  (open scsh-user
        debug-messages
        test-base)
  (files terminal-device-control-test))

;; 
(define-structure test-all-syscalls
  (export test-all)
  (open scheme
        test-base
        file-system-test 

        process-state-test
        user-and-group-db-access-test
        system-parameter-test
        datetime-test
        envvar-test

        ; terminal-device-control-test
      ))

;; --------------------------------------------------

(define-structure file-name-maniplation-test (export)
  (open scsh-user
	test-base)
  (files file-name-manipulation-test))

(define-structure read-delimited-strings-test (export)
  (open scsh-user
	test-base)
  (files read-delimited-strings))

(define-structure bitwise-ops-test (export)
  (open scsh-user
        test-base)
  (files bitwise-ops-test))

(define-structure test-all
  (export test-all)
  (open scheme
        test-base
        
        test-all-syscalls
        file-name-maniplation-test
        read-delimited-strings-test
        bitwise-ops-test

      ))

