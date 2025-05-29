#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title{Processes}

@deftogether[(@defproc/nolink[(exec [program string?] [arg string?] ...) any]
              @defproc/nolink[(exec-path [program string?] [arg string?] ...) any]
              @defproc/nolink[(exec/env [program string?] [environment (or/c (listof (cons/c string? string?)) #t)] [arg string?] ...) any]
              @defproc/nolink[(exec-path/env [program string?] [environment (or/c (listof (cons/c string? string?)) #t)] [arg string?] ...) any])]{
The @code{.../env} variants take an environment specified as a string to string alist. An
environment of @code{#t} is taken to mean the current process' environment (i.e., the value of the
external char @code{**environ}).

[Rationale: @code{#f} is a more convenient marker for the current environment than @code{#t}, but
would cause an ambiguity on Schemes that identify @code{#f} and @code{()}.]

The path-searching variants search the directories in the list @code{exec-path-list} for the
program. A path-search is not performed if the program name contains a slash character---it is used
directly. So a program with a name like @code{"bin/prog"} always executes the program
@code{bin/prog} in the current working directory. See @code{$path} and @code{exec-path-list}, below.

Note that there is no analog to the C function @code{execv()}. To get the effect just do

@codeblock{(apply exec prog arglist)}

All of these procedures flush buffered output and close unrevealed ports before executing the new
binary. To avoid flushing buffered output, see @code{%exec} below.

Note that the C @code{exec()} procedure allows the zeroth element of the argument vector to be
different from the file being executed, e.g.

@codeblock{char *argv[] = {"-", "-f", 0};
           exec("/bin/csh", argv, envp);}

The scsh @code{exec}, @code{exec-path}, @code{exec/env}, and @code{exec-path/env} procedures do not
give this functionality---element 0 of the arg vector is always identical to the @code{prog}
argument. In the rare case the user wishes to differentiate these two items, he can use the
low-level @code{%exec} and @code{exec-path-search} procedures.

These procedures never return under any circumstances. As with any other system call, if there is an
error, they raise an exception.
}

@deftogether[(@defproc/nolink[(%exec [program string?] [arglist (listof string?)] [env (or/c #t (listof (pair/c string? string?)))]) any]
              @defproc/nolink[(exec-path-search [fname string?] [pathlist (listof string?)]) (or/c string? #f)])]{
The @code{%exec} procedure is the low-level interface to the system call. The @var{arglist}
parameter is a list of arguments; @var{env} is either a string to string alist or @code{#t}. The new
program's @code{argv[0]} will be taken from @code{(car arglist)}, @emph{not} from @var{prog}. An
environment of @code{#t} means the current process' environment. @code{%exec} does not flush
buffered output (see @code{flush-all-ports}).

All exec procedures, including @code{%exec}, coerce the @var{prog} and @var{arg} values to strings
using the usual conversion rules: numbers are converted to decimal numerals, and symbols converted
to their print-names.

@code{exec-path-search} searches the directories of @var{pathlist} looking for an occurrence of
file @code{fname}. If no executable file is found, it returns @code{#f}. If @var{fname} contains a
slash character, the path search isshort-circuited, but the procedure still checks to ensure that
the file exists and is executable---if not, it still returns @code{#f}. Users of this procedure
should be aware that it invites a potential race condition: between checking the file with
@code{exec-path-search} and executing it with @code{%exec}, the file's status might change. The only
atomic way to do the search is to loop over the candidate file names, exec'ing each one and looping
when the exec operation fails.

See @code{$path} and @code{exec-path-list}, below.
}

@deftogether[(@defproc/nolink[(exit [status integer? 0]) any]
              @defproc/nolink[(%exit [status integer? 0]) any])]{
These procedures terminate the current process with a given exit status. The default exit status is
0. The low-level @code{%exit} procedure immediately terminates the process without flushing buffered
output.
}

@defproc/nolink[(call-terminally [thunk (-> (values any ...))]) any]{
@code{call-terminally} calls its thunk. When the thunk returns, the process exits. Although
@code{call-terminally} could be implemented as

@codeblock{(lambda (thunk) (thunk) (exit 0))}

an implementation can take advantage of the fact that this procedure never returns. For example, the
runtime can start with a fresh stack and also start with a fresh dynamic environment, where shadowed
bindings are discarded. This can allow the old stack and dynamic environment to be collected
(assuming this data is not reachable through some live continuation).
}

@defproc/nolink[(suspend) undefined]{
Suspend the current process with a SIGSTOP signal.
}

@deftogether[(@defproc/nolink[(fork [thunk (or/c #f (-> (values any ...)))] [continue-threads? boolean? #f]) (or/c proc? #f)]
              @defproc/nolink[(%fork [thunk (or/c #f (-> (values any ...)))] [continue-threads? boolean? #f]) (or/c proc? #f)])]{
@code{fork} with no arguments or @code{#f} instead of a thunk is like C @code{fork()}. In the parent
process, it returns the child's @emph{process object} (see below for more information on process
objects). In the child process, it returns @code{#f}.

@code{fork} with an argument only returns in the parent process, returning the child's process
object. The child process calls @var{thunk} and then exits.

@code{fork} flushes buffered output before forking, and sets the child process to non-interactive.
@code{%fork} does not perform this bookkeeping; it simply forks.

The optional boolean argument @var{continue-threads?} specifies whether the currently active threads
continue to run in the child or not. The default is @code{#f}.
}

@deftogether[(@defproc/nolink[(fork/pipe [thunk (or/c #f (-> (values any ...)))] [continue-threads? boolean? #f]) (or/c proc? #f)]
              @defproc/nolink[(%fork/pipe [thunk (or/c #f (-> (values any ...)))] [continue-threads? boolean? #f]) (or/c proc? #f)])]{
Like @code{fork} and @code{%fork}, but the parent and child communicate via a pipe connecting the
parent's stdin to the child's stdout. These procedures side-effect the parent by changing his stdin.

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
@code{set-current-input-port!} or a related form. Here is an example showing how to set up the I/O
ports on both sides of the pipe:

@codeblock{(fork/pipe (lambda ()
                        (with-current-output-port (fdes->outport 1)
                        (display "Hello, world.\\n"))))
            (set-current-input-port! (fdes->inport 0))
            (read-line)     ; Read the string output by the child.}

None of this is necessary when the I/O is performed by an exec'd program in the child or parent
process, only when the pipe will be referenced by Scheme code through one of the default current I/O
ports.
}

@deftogether[(@defproc/nolink[(fork/pipe+ [conns (listof integer?)] [thunk (or/c #f (-> (values any ...)))] [continue-threads? boolean? #f]) (or/c proc? #f)]
              @defproc/nolink[(%fork/pipe+ [conns (listof integer?)] [thunk (or/c #f (-> (values any ...)))] [continue-threads? boolean? #f]) (or/c proc? #f)])]{
Like @code{fork/pipe}, but the pipe connections between the child and parent are specified by the
connection list @var{conns}. See the

@codeblock{("|+" conns pf1 ... pfn)}

process form for a description of connection lists.
}

@section{Process objects and process reaping}
@subsection{Issues with process reaping}
@section{Process waiting}
@section{Analysing process status codes}