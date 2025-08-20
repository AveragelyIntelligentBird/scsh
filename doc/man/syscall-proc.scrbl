#lang scribble/manual

@title{Processes}

@deftogether[(@defproc[(exec      [program (or symbol? string?)] 
                                  [arg string?] ...) (values <no values returned>)]
              @defproc[(exec-path [program (or symbol? string?)]
                                  [arg string?] ...) (values <no values returned>)]
              @defproc[(exec/env  [program [program (or symbol? string?)]] 
                                  [env (or (alist string->string) #f)] 
                                  [arg string?] ...) (values <no values returned>)]
              @defproc[(exec-path/env [program [program (or symbol? string?)]] 
                                  [env (or (alist string->string) #f)] 
                                  [arg string?] ...) (values <no values returned>)])]{
  This is the scsh interface for @code{exec()}.

  @margin-note{
  Used to be @code{#t} as a marker. @code{#f} is a more convenient marker for the current environment 
  than @code{#t}, but would cause an ambiguity on Schemes that identify @code{#f} and @code{()}.
  }

  The @code{.../env} variants take an environment @var{env} specified as a string to string alist. An
  environment @var{env} of @code{#f} is taken to mean the current process' environment 
  (i.e., the value of the external @code{char **environ}).

  The path-searching variants search the directories in the list @code{exec-path-list} for the
  program. A path-search is not performed if the program name contains a slash character---it is used
  directly. So a program with a name like @code{"bin/prog"} always executes the program
  @code{bin/prog} in the current working directory. See more details about @code{$path} and 
  @code{exec-path-list} @seclink["path-list-sect"]{here}.

  Note that there is no analog to the C function @code{execv()}. To get the effect just do
  @codeblock{(apply exec prog arglist)}

  All of these procedures flush buffered output and close unrevealed ports before executing the new
  binary. To avoid flushing buffered output, see @code{%exec} below.

  Note that the C @code{exec()} procedure allows the zeroth element of the argument vector to be
  different from the file being executed, e.g.

  @codeblock{
  char *argv[] = {"-", "-f", 0};
  exec("/bin/csh", argv, envp);
  }

  The scsh @code{exec}, @code{exec-path}, @code{exec/env}, and @code{exec-path/env} procedures do not
  give this functionality---element 0 of the arg vector is always identical to the @code{prog}
  argument. In the rare case the user wishes to differentiate these two items, he can use the
  low-level @code{%exec} and @code{exec-path-search} procedures.

  These procedures never return under any circumstances. As with any other system call, if there is an
  error, they raise an exception.
}

@deftogether[(@defproc[(%exec [program (or symbol? string?)] 
                              [arglist (listof (or symbol? string?))] 
                              [env (or (alist string->string) #f)]) (values <no values returned>)]
              @defproc[(exec-path-search [fname string?] 
                                         [pathlist (listof string?)]) (or/c string? #f)])]{
The @code{%exec} procedure is the low-level interface to the system call. The @var{arglist}
parameter is a list of arguments; @var{env} is either a string to string alist or @code{#f}. 

The new program's @code{argv[0]} will be taken from @code{(car arglist)}, @emph{not} from @var{prog}. 

An environment of @code{#f} means the current process' environment. @code{%exec} does not flush
buffered output.

All exec procedures, including @code{%exec}, coerce the @var{prog} and @var{arg} values to strings
using the usual conversion rules: numbers are converted to decimal numerals, and symbols converted
to their print-names.

@code{exec-path-search} searches the directories of @var{pathlist} looking for an occurrence of
file @code{fname}. If no executable file is found, it returns @code{#f}. If @var{fname} contains a
slash character, the path search is short-circuited, but the procedure still checks to ensure that
the file exists and is executable---if not, it still returns @code{#f}. 

Users of this procedure should be aware that it invites a potential race condition: between 
checking the file with @code{exec-path-search} and executing it with @code{%exec}, the file's 
status might change. The only atomic way to do the search is to loop over the candidate file names, 
exec'ing each one and looping when the exec operation fails.

See more details about @code{$path} and @code{exec-path-list} @seclink["path-list-sect"]{here}.
}

@deftogether[(@defproc[(exit [status integer? 0]) any]
              @defproc[(%exit [status integer? 0]) any])]{
These procedures terminate the current process with a given exit status. The default exit status is
0. The low-level @code{%exit} procedure immediately terminates the process without flushing buffered
output.
}

@defproc[(call-terminally [thunk (-> any)]) (values value/s of thunk)]{
@code{call-terminally} calls its thunk. When the thunk returns, the process exits. Although
@code{call-terminally} could be implemented as

@codeblock{(lambda (thunk) (thunk) (exit 0))}

an implementation can take advantage of the fact that this procedure never returns. For example, the
runtime can start with a fresh stack and also start with a fresh dynamic environment, where shadowed
bindings are discarded. This can allow the old stack and dynamic environment to be collected
(assuming this data is not reachable through some live continuation).
}

@deftogether[(@defproc[(fork  [thunk (or (-> any) #f) #f]) (values proc or #f)]
              @defproc[(%fork [thunk (or (-> any) #f) #f]) (values proc or #f)])]{

@margin-note{
In previous releases there was an extra argument for independtently specifying whether the currently
active threads continue to run in the child or not. This might be brought back in future releases. 
}

@margin-note{
Note that these forking procedures concern @emph{process} forking. Scsh also offers "thread forking", 
which preserves thread-local fluids. For more details, see @secref["thread-local-sec"].
}

@code{fork} with no arguments or @code{#f} instead of a thunk is like C @code{fork()}. In the parent
process, it returns the child's @emph{process object} (see @seclink["proc-obj-sec"]{below} for more 
information on process objects). In the child process, it returns @code{#f}. Note that the forked 
child process contains all the treads of the parent process.

@code{fork} with an argument @var{thunk} can be throught of as "forking off just the given thunk". 
In the parent process, the procedure similarly returns the child's process object. In the child 
process, however, it immediately calls @var{thunk} with @code{call-terminally} and then exits. 
To ensure that nothing but the thunk executes, we disable the thread system in the child process.

@code{fork} flushes buffered output before forking, and sets the child process to non-interactive mode
(known as "batch mode" in scheme48). @code{%fork} does not perform any bookkeeping; it simply forks.
}

@deftogether[(@defproc[(fork/pipe  [thunk (or (-> any) #f) #f]) (values proc or #f)]
              @defproc[(%fork/pipe [thunk (or (-> any) #f) #f]) (values proc or #f)])]{
Like @code{fork} and @code{%fork}, but the parent and child communicate via a pipe connecting the
parent's stdin to the child's stdout. These procedures side-effect the parent by changing its stdin.

In effect, @code{fork/pipe} splices a process into the data stream immediately upstream of the
current process. This is the basic function for creating pipelines. Long pipelines are built by
performing a sequence of @code{fork/pipe} calls. For example, to create a background two-process
pipe @code{"a | b"}, we write:

@codeblock{(fork (lambda () (fork/pipe a) (b)))}

which returns the process object for @code{b}'s process.

To create a background three-process pipe @code{"a | b | c"}, we write:

@codeblock{(fork (lambda () (fork/pipe a)
                            (fork/pipe b)
                            (c)))}

which returns the process object for @code{c}'s process.

Note that these procedures affect file descriptors, not ports. That is, the pipe is allocated
connecting the child's file descriptor 1 to the parent's file descriptor 0. @emph{Any previous
Scheme port built over these affected file descriptors is shifted to a new, unused file descriptor
with @code{dup} before allocating the I/O pipe.} This means, for example, that the ports bound to
@code{(current-input-port)} and @code{(current-output-port)} in either process are not
affected---they still refer to the same I/O sources and sinks as before. Remember the simple scsh
rule: Scheme ports are bound to I/O sources and sinks, @emph{not} particular file descriptors.

If the child process wishes to rebind the current output port to the pipe on file descriptor 1, it
can do this using @code{with-current-output-port} or a related form. Similarly, if the parent wishes
to change the current input port to the pipe on file descriptor 0, it can do this using
@code{with-current-input-port} or a related form. Here is an example showing how to set up the I/O
ports on both sides of the pipe:

@codeblock{
  > (fork/pipe (lambda ()
      (with-current-output-port (fdes->outport 1)
        (display "Hello, world.\\n"))))
  > (with-current-input-port (fdes->inport 0)
      (read-line))  ; Read the string output by the child.
}

None of this is necessary when the I/O is performed by an exec'd program in the child or parent
process, only when the pipe will be referenced by Scheme code through one of the default current I/O
ports.
}

@deftogether[(@defproc[(fork/pipe+  [conns (listof integer?)] [thunk (or (-> any) #f) #f]) (values proc or #f)]
              @defproc[(%fork/pipe+ [conns (listof integer?)] [thunk (or (-> any) #f) #f]) (values proc or #f)])]{
Like @code{fork/pipe}, but the pipe connections between the child and parent are specified by the
connection list @var{conns}. See the

@codeblock{("|+" conns pf1 ... pfn)}

process form for a description of connection lists @seclink["proc-forms-sec"]{here}.
}

@section[#:tag "proc-obj-sec"]{Process objects and process reaping}
Scsh uses @as-index[@code{proc}] records to represent Unix processes, which we call 
@emph{process objects}. They are created by the @code{fork} procedure, and have the following exposed 
structure:
@codeblock{
    (define-record-type 
      ; ... ;
      proc?
      (pid proc:pid)) 
}

The record is discriminated with the @as-index[@code{proc?}] predicate. The only exposed slot in a 
@code{proc} record is the process' pid, the integer id assigned by Unix to the process. Is's accessor 
is @code{proc:pid}. 

@defproc[(pid->proc [pid integer?]
                    [probe? any #f]) proc]{
  This procedure maps integer Unix process ids to scsh process objects. It is intended for use mainly
  in interactive and debugging code. 
  
  If there is no process object in the system indexed by the given @var{pid}, @code{pid->proc}'s 
  action is determined by the @var{probe?} parameter, which defaults to #f.
  @tabular[#:style 'boxed
           #:row-properties '((bottom-border top-border) '())
           #:sep @hspace[3]
    (list (list "" @var{probe?} @bold{Return})
      (list "" @code{#f}      "Signals error condition.")
      (list "" @code{'create} "Create new proc object.")
      (list "" @emph{other}   @code{#f}))]
}

Sometime after a child process terminates, scsh will perform a wait system call on the child in 
background, caching the process' exit status in the child's proc object. This is called "reaping" 
the process. Once the child has been waited, the Unix kernel can free the storage allocated for the 
dead process' exit information, so process reaping prevents the process table from becoming 
cluttered with un-waited dead child processes (a.k.a. "zombies"). This can be especially severe if 
the scsh process never waits on child processes at all; if the process table overflows with forgotten 
zombies, the OS may be unable to fork further processes.

Reaping a child process moves its exit status information from the kernel into the scsh process, 
where it is cached inside the child's process object. If the scsh user drops all pointers to the 
process object, it will simply be garbage collected. On the other hand, if the scsh program retains 
a pointer to the process object, it can use scsh's wait system call to synchronise with the child 
and retrieve its exit status multiple times (this is not possible with simple Unix integer pids in 
C -- the programmer can only wait on a pid once).

Thus, process objects allow scsh programmer to do two things not allowed in other programming 
environments:
@itemlist[
  @item{Subprocesses that are never waited on are still removed from the process table, and their 
        associated exit status data is eventually automatically garbage collected.}
  @item{Subprocesses can be waited on multiple times.}]

However, note that once a child has exited, if the scsh programmer drops all pointers to the child's
proc object, the child's exit status will be reaped and thrown away. This is the intended behaviour,
and it means that integer pids are not enough to cause a process's exit status to be retained by 
the scsh runtime. (This is because it is clearly impossible to GC   data referenced by integers.)

As a convenience for interactive use and debugging, all procedures that take process objects will 
also accept integer Unix pids as arguments, coercing them to the corresponding process objects. 
Since integer process ids are not reliable ways to keep a child's exit status from being reaped and 
garbage collected, programmers are encouraged to use process objects.

@defproc[(autoreap-policy [policy policy?]) old-policy]{
  The scsh programmer can choose different policies for automatic process reaping. 
  The policy is determined by applying this procedure to one of the values 'early, 'late, or #f
  (i.e., no autoreap).
  @tabular[#:style 'boxed
           #:row-properties '((bottom-border top-border) '())
           #:sep @hspace[3]
    (list (list "" @var{policy} @bold{Return})
      (list "" @code{'early} "The child is reaped from the Unix kernel's process table into scsh as 
                              soon as it dies. This is done by having a signal handler for the SIGCHLD
                              signal reap the process.")
      (list "" @code{'late}  "The child is not autoreaped until it dies and the scsh program drops 
                              all pointers to its process object. That is, the process table is 
                              cleaned out during garbage collection.")
      (list "" @code{#f}     "If autoreaping is turned off, process reaping is completely under 
                              control of the programmer, who can force outstanding zombies to be 
                              reaped by manually calling the reap-zombies procedure (see below)."))]

  Note that under any of the autoreap policies, a particular process p can be manually reaped into 
  scsh by simply calling @code{(wait p)}. All zombies can be manually reaped with @code{(reap-zombies)}.
 
  The @code{autoreap-policy} procedure returns the policy's previous value. Calling autoreap-policy 
  with no arguments returns the current policy without no change.
}      

@defproc[(reap-zombies) boolean]{
  This procedure reaps all outstanding exited child processes into scsh. It returns true if there 
  are no more child processes to wait on, and false if there are outstanding processes still 
  running or suspended. 
}
    
@subsection{Issues with Reaping}

Reaping a process does not reveal its process group at the time of death; this information is lost 
when the process reaped. This means that a dead, reaped process is not eligible as a return value for
a future @code{wait-process-group} call. This is not likely to be a problem for most code, as 
programs almost never wait on exited processes by process group. Process group waiting is usually 
applied to stopped processes, which are never reaped. So it is unlikely that this will be a problem 
for most programs.

Automatic process reaping is a useful programming convenience. However, if a program is careful to 
wait for all children, and does not wish automatic reaping to happen, the programmer can simply turn 
process autoreaping off.

Programs that do not wish to use automatic process reaping should be aware that some scsh routines 
create subprocesses but do not return the child's pid: @code{run/port*}, and its related procedures and 
special forms (@code{run/strings}, et al.). Automatic process reaping will clean the child processes 
created by these procedures out of the kernel's process table. If a program doesn't use process 
reaping, it should either avoid these forms, or use @code{wait-any} to wait for the children to exit.

@section{Process waiting}

@defproc[(wait [proc/pid (or proc? integer?)] [flags integer? 0]) status]{
  
  This procedure waits until a child process exits, and returns its exit code. The 
  @var{proc/pid} argument is either a process object (see @seclink["proc-obj-sec"]{here}) or an 
  integer process id. @code{wait} returns the child's exit status code (or suspension code, if 
  the @code{wait/stopped-children} option is used, see below). 
  
  Status values can be queried with the procedures in @secref["analyzing-procs-sec"].

  The @var{flags} argument is an integer whose bits specify additional options. 
  It is composed by or'ing together the following flags:
  @tabular[#:style 'boxed
           #:row-properties '((bottom-border top-border) '())
           #:sep @hspace[3]
    (list (list "" @bold{Wait flag} @bold{Unix flag} @bold{Description})
      (list "" @as-index{@code{wait/poll}}             @code{WNOHANG}    
            "Return #f immediately if child still active.")
      (list "" @as-index{@code{wait/stopped-children}} @code{WUNTRACED}  
            "Wait for suspend as well as exit.")
      (list "" @as-index{@code{wait/cont-children}}    @code{WCONTINUED} 
            "Wait for children being resumed with SIGCONT as well as exit."))]
}      
   	
@defproc[(wait-any [flags integer? 0]) (values [proc status])]{
  The optional @var{flags} argument is as for @code{wait}. This procedure waits for any child process 
  to exit (or stop, or continue). It returns the process' process object and status code. 
  
  If there are no children left for which to wait, the two values [#f #t] are returned. 
  If the wait/poll flag is used, and none of the children are immediately eligble for waiting, 
  then the values [#f #f] are returned:
    [#f #f] 	Poll, none ready
    [#f #t] 	No children

  @code{wait-any} will not return a process that has been previously waited by any other process-wait 
  procedure (@code{wait}, @code{wait-any}, and @code{wait-process-group}). It will return reaped 
  processes that haven't yet been waited.

  The use of wait-any is deprecated.
}   

@defproc[(wait-process-group [proc/pid (or proc? integer?)] [flags integer? 0]) (values [proc status])]{
  This procedure waits for any child whose process group is @var{proc/pid} (either a process object or
  a pid). The @var{flags} argument is as for wait.

  TODO LIES LIES LIES

  Note that if the programmer wishes to wait for exited processes by process group, the program 
  should take care not to use process reaping (section 3.4.1), as this 
  loses process group information. However, most process-group waiting is for stopped 
  processes (to implement job control), so this is rarely an issue, as stopped processes are not 
  subject to reaping.
}  
    
@section[#:tag "analyzing-procs-sec"]{Analysing process status codes}

When a child process dies (or is suspended), its parent can call the wait procedure to recover the 
exit (or suspension) status of the child. The exit status is a small integer that encodes information 
describing how the child terminated. The bit-level format of the exit status is not defined by 
POSIX; you must use the following three functions to decode one. However, if a child terminates 
normally with exit code 0, POSIX does require wait to return an exit status that is exactly zero. 
So (zero? status) is a correct way to test for non-error, normal termination, e.g.,
@codeblock{
  (if (zero? (run (rcp scsh.tar.gz lambda.csd.hku.hk:)))
        (delete-file "scsh.tar.gz"))
}

@deftogether[(
  @defproc[(status:exit-val [status status?]) (values integer or #f)]
  @defproc[(status:stop-sig [status status?]) (values integer or #f)]
  @defproc[(status:term-sig [status status?]) (values integer or #f)])]{

  For a given status value produced by calling @code{wait}, exactly one of these routines will return 
  a true value.

  If the child process exited normally, @code{status:exit-val} returns the exit code for the child 
  process (i.e., the value the child passed to exit or returned from main). Otherwise, this 
  function returns @code{#f}

  If the child process was suspended by a signal, @code{status:stop-sig} returns the signal that 
  suspended the child. Otherwise, this function returns false.

  If the child process terminated abnormally, @code{status:term-sig} returns the signal that 
  terminated the child. Otherwise, this function returns false.
}

@section{Miscellaneous}

@defproc[(suspend) unspecific]{
  Suspend all processes in the current process group with a @code{SIGSTOP} signal. Equivalent to 
  @codeblock{(signal-process 0 (signal stop))}
}

@deftogether[(@defproc[(process-sleep       [secs integer]) boolean]
              @defproc[(process-sleep-until [time integer]) boolean])]{
  The @code{process-sleep} procedure causes the process to sleep for @var{secs} seconds. 
  The @code{process-sleep-until} procedure causes the process to sleep until @var{time} (see 
  @seclink["datetime-chap"]{here} for more details).

  Note that these procedures suspend all running threads, including the ones Scsh uses for 
  administrtive purposes. Consider using the @code{sleep} procedure to only put the current thread
  to sleep. For more details, see @secref["threads-chapter"].
}