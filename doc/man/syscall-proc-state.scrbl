#lang scribble/manual

@title{Process state}

@deftogether[(@defproc[(umask) file-mode]
              @defproc[(set-umask   [perms (or file-mode? integer?)]) undefined]
              @defproc[(with-umask* [perms (or file-mode? integer?)] [thunk (-> any)]) (values value/s of thunk)]
              @defform[(with-umask  [perms (or file-mode? integer?)] body ...+)])]{
  The process' current umask is retrieved with @code{(umask)}, and set with @code{(set-umask perms)}.
  @var{perms} can be either a fixnum or a @code{file-mode} (see more @seclink["file-modes-sec"]{here}). 
  
  Calling @code{with-umask*} changes the umask to perms for the duration of the call to @var{thunk}. 
  If the program throws out of @var{thunk} by invoking a continuation, the umask is reset to its 
  external value. If the program throws back into thunk by calling a stored continuation, 
  the umask is restored to the @var{perms} value. 
  
  The special form @code{with-umask} is equivalent in effect to the procedure @code{with-umask*}, 
  but does not require the programmer to explicitly wrap a @code{(lambda () ...)} around the body of 
  the code to be executed. 
}

@deftogether[(@defproc[(cwd) string]
              @defproc[(chdir [path string? home-directory]) undefined]
              @defproc[(with-cwd* [path string?] [thunk (-> any)]) (values value/s of thunk)]
              @defform[(with-cwd  [path string?] body ...+)])]{
  The process' current working directory is retrieved with @code{(cwd)}, and set with 
  @code{(chdir path)}, although in most cases, @code{with-cwd} is preferrable. If @code{chdir} is 
  called with no arguments, it changes the cwd to the user's home directory. 
  
  The @code{with-cwd*} procedure calls @var{thunk} with the cwd temporarily set to @var{path}; 
  when thunk returns, or is exited in a non-local fashion (e.g., by raising an exception or by 
  invoking a continuation), the cwd is returned to its original value. 
  
  The special form @code{with-cwd} is simply syntactic sugar for @code{with-cwd*}. 
}

@deftogether[(@defproc[(pid) fixnum]
              @defproc[(parent-pid) fixnum]
              @defproc[(process-group) fixnum]
              @defproc*[([(set-process-group [process-group integer?]) undefined]
                         [(set-process-group [proc/pid (or integer? proc-obj?)] [process-group integer?]) undefined])])]{
  @code{(pid)} and @code{(parent-pid)} retrieve the process id for the current process and its parent.
  @code{(process-group)} returns the process group of the current process. 
  
  A process' process group can be set with @code{set-process-group}. @var{process-group} specifies the 
  target process group. One-arguement version of the procedure acts on the current process, while 
  the two argument version uses @var{proc/pid} argument to specify the affected process. 
  It may be either an integer process id or a process object (see more @seclink["proc-obj-sec"]{here}).
}

@; TODO priority stuff?
@; (set-priority which who priority)     --->     undefined         (procedure) 
@; (priority which who)     --->     fixnum         (procedure) 
@; (nice [proc/pid delta])     --->     undefined         (procedure) 

@deftogether[(@defproc[(user-uid) integer]
              @defproc[(set-uid [uid integer?]) undefined]
              @defproc[(user-effective-uid) integer]
              @defproc[(set-user-effective-uid [uid integer?]) undefined]
              @defproc[(with-user-effective-uid* [uid integer?] [thunk (-> any)]) (values value/s of thunk)]
              @defform[(with-user-effective-uid  [uid integer?] body ...+)])]{
  These routines get and set the effective and real user ids. The @code{set-uid} routine correspond to
  the POSIX @code{setuid()} procedure.

  Possible values for setting this resource are either the real user ID or the saved set-user-ID. 
  The @code{with-...} forms perform the ususal temporary assignment during the execution of the 
  second argument. The effective user ID is thread-local. 
}

@deftogether[(@defproc[(user-gid) integer]
              @defproc[(user-supplementary-gids) (values fixnum list)]
              @defproc[(set-gid [uid integer?]) undefined]
              @defproc[(user-effective-gid) integer]
              @defproc[(set-user-effective-gid [gid integer?]) undefined]
              @defproc[(with-user-effective-gid* [gid integer?] [thunk (-> any)]) (values value/s of thunk)]
              @defform[(with-user-effective-gid  [gid integer?] body ...+)])]{
  Similar to above, except these procedures get and set the effective and real @emph{group} ids. 
  The @code{set-gid} routine correspond to the POSIX @code{setgid()} procedure.

  Possible values for setting this resource are either the real group ID or the saved set-group-ID. 
  The @code{with-...} forms perform the ususal temporary assignment during the execution of the 
  second argument. The effective group ID is thread-local. 
}

@defproc[(process-times) (values [integer integer integer integer])]{
  Returns four values:
  @itemlist[@item{user CPU time in clock-ticks}
            @item{system CPU time in clock-ticks}
            @item{user CPU time of all descendant processes}
            @item{system CPU time of all descendant processes}]

  Note that CPU time clock resolution is not the same as the real-time clock resolution 
  provided by @code{(time+ticks)}. That's Unix. 
}
    
@defproc[(cpu-ticks/sec) integer]{
  Returns the resolution of the CPU timer in clock ticks per second. 
  This can be used to convert the times reported by process-times to seconds.  
} 
