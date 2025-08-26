;;; File Status Flags ---------------------------------------------------------
;; Part of scsh 0.7. See file COPYING for notices and license.
;; Describes enumerated set type for file status flags for open() and fcntl()
;; and defines legacy bindings. 

(define-enumerated-type file-flag :file-flag
  file-flag?				  ; Predicate
  the-file-flags		  ; Vector containing all elements
  file-flag-name			; Name accessor
  file-flag-index			; Index accessor
  ;; The order of these is known to the C code
  ( 
    ;; Access modes, exactly one required for open()
    ;; Considered a file status flag, so it can be read by fcntl()
    read-write
    write-only
    read-only
    
    ;; File creation flags, only affect open()
    cloexec    ; Automatically	close file on execve(2)
               ; Avoids potential race condition if you were to use fcntl() instead
    create     ; Create file if it does not exist
    directory  ; Error	if file	is not a directory
    exclusive  ; Fail if O_CREAT is set and the file exists
    no-controlling-tty  ; If pathname refers to a terminal device, it will not become 
                        ; the process's controlling terminal
    no-follow  ; Do not follow	symlinks
    truncate   ; Truncate size	to 0
    ; tmp-file   ; NOT IN BSD, create an unnamed temporary regular file
    ; path       ; NOT IN BSD, The file itself is not opened but you get an fd
    ; large-file ; NOT IN BSD, allow files whose sizes cannot be represented in an
    ;            ;  off_t (but can be represented in an off64_t) to be opened.
      
    ;; File status flags, read and written by fcntl()
    append
    async
    direct     ; TODO: Couldn't find it in apple-oss's fcntl.h, but that's like outdated + freeBSD has it
    data-sync  ; DSYNC, synchronized I/O *data* integrity completion.
    file-sync  ; FSYNC/SYNC, synchronized I/O *file* integrity completion
               ; (incorrectly AKA RSYNC in Linux)
    ; no-access-time ; NOT IN BSD, do not update the file last access time
    nonblocking
    
))

(define-enum-set-type file-flags :file-flags
  file-flags?
  make-file-flags

  file-flag file-flag?
  the-file-flags
  file-flag-index)

(define-exported-binding "file-flags-enum-set-type" :file-flags)

(define (file-flags-on? checked-flags target-flags)
  (enum-set-subset? target-flags checked-flags))

(define (file-flags+ flags0 flags1)
  (enum-set-union flags0 flags1))

(define (file-flags- flags0 flags1)
  (enum-set-difference flags0 flags1))

(define access-mode-mask 
  (file-flags read-write write-only read-only))

(define creation-flags-mask 
  (file-flags 
    cloexec 
    create 
    directory 
    exclusive 
    no-controlling-tty 
    no-follow
    truncate 
    ; tmp-file 
    ; path 
    ; large-file
  ))

(define status-flags-mask 
  (file-flags 
    append 
    async 
    direct 
    ; no-access-time 
    nonblocking 
    data-sync 
    file-sync))

(define (mask-file-flags flags mask)
  (enum-set-intersection flags mask))

(define (file-access-mode flags)
  (mask-file-flags flags access-mode-mask))

(define (file-flags->integer flags)
  (%file-flags->int flags))

(define (integer->file-flags fixnum)
  (%int->file-flags fixnum))
