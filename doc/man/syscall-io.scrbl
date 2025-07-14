#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title{I/O}

<current approach is fully modular with regards to s48, it is not optimized for speed>

@section{Standard RnRS I/O Procedures}
In scsh, most standard RnRS I/O operations (such as @code{display} or @code{read-char}) work on both
integer file descriptors and Scheme ports. When doing I/O with a file descriptor, the I/O operation
is done directly on the file, bypassing any buffered data that may have accumulated in an associated
port. Note that character-at-a-time operations such as @code{read-char} are likely to be quite slow
when performed directly upon file descriptors.

The standard RnRS procedures @code{read-char}, @code{char-ready?}, @code{write}, @code{display},
@code{newline}, and @code{write-char} are all generic, accepting integer file descriptor arguments
as well as ports. Scsh also mandates the availability of @code{format}, and further requires
@code{format} to accept file descriptor arguments as well as ports.

The procedures @code{peek-char} and @code{read} do @emph{not} accept file descriptor arguments,
since these functions require the ability to read ahead in the input stream, a feature not supported
by Unix I/O.

@section{Port Manipulation and Standard Ports}
@defproc/nolink[(close-after [port port?] [consumer (-> port? any)]) any]{
Returns @code{(consumer port)}, but closes the port on return. No dynamic-wind magic.
}

@defproc/nolink[(error-output-port) port?]{
This procedure is analogous to @code{current-output-port}, but produces a port used for error
messages---the scsh equivalent of stderr.
}

@deftogether[(@defproc/nolink[(with-current-input-port* [port port?] [thunk (-> any)]) any]
              @defproc/nolink[(with-current-output-port* [port port?] [thunk (-> any)]) any]
              @defproc/nolink[(with-error-output-port* [port port?] [thunk (-> any)]) any])]{
These procedures install @var{port} as the current input, current output, and error output port,
respectively, for the duration of a call to @var{thunk} and return @var{thunk}'s value(s).
}

@deftogether[(@defform/nolink[(with-current-input-port port body ...+)]
              @defform/nolink[(with-current-output-port port body ...+)]
              @defform/nolink[(with-error-output-port port body ...+)])]{
These special forms are simply syntactic sugar for the @code{with-current-input-port*} procedure and
friends.
}

@defproc/nolink[(close [fd/port (or/c integer? port?)]) boolean?]{
Closes the port or file descriptor.

If @var{fd/port} is a file descriptor, and it has a port allocated to it, the port is shifted to a
new file descriptor created with @code{(dup fd/port)} before closing @code{fd/port}. The port then
has its revealed count set to zero.  This reflects the design criteria that ports are not associated
with file descriptors, but with the streams they denote.

To close a file descriptor, and any associated port it might have, you must instead say one of
(as appropriate):

@codeblock{(close (fdes->inport  fd))
           (close (fdes->outport fd))}

The procedure returns true if it closed an open port. If the port was already closed, it returns
false; this is not an error.
}

@defproc/nolink[(stdports->stdio) undefined]{
Synchronises Unix' standard I/O file descriptors and Scheme's current I/O ports. Causes the standard
I/O file descriptors (0, 1, and 2) take their values from the current I/O ports. It is exactly
equivalent to the series of redirections:

@codeblock{(dup (current-input-port)  0)
           (dup (current-output-port) 1)
           (dup (error-output-port)   2)}

@margin-note{Why not @code{move->fdes}? Because the current output port and error port might be the
same port.}
}

@deftogether[(@defproc/nolink[(with-stdio-ports* [thunk (-> any)]) any]
              @defform/nolink[(with-stdio-ports body ...+)])]{
@code{with-stdio-ports*} binds the standard ports @code{(current-input-port)},
@code{(current-output-port)}, and @code{(error-output-port)} to be ports on file descriptors 0, 1,
2, and then calls @var{thunk}. It is equivalent to:

@codeblock{(with-current-input-port (fdes->inport 0)
             (with-current-output-port (fdes->inport 1)
               (with-error-output-port (fdes->outport 2)
                 (thunk))))}

The @code{with-stdio-ports} special form is merely syntactic sugar.
}

@section{String ports}
Scheme48 has string ports, which you can use. Scsh has not committed to the particular interface or
names that scheme48 uses, so be warned that the interface described herein may be liable to change.
@; ^ isn't there a SRFI for this? change wording or remove if so


@defproc/nolink[(make-string-input-port [string string?]) port?]{
Returns a port that reads characters from the supplied string.
}

@deftogether[(@defproc/nolink[(make-string-output-port) port?]
              @defproc/nolink[(string-output-port-output [port port?]) string?])]{
A string output port is a port that collects the characters given to it into a string. The
accumulated string is retrieved by applying @code{string-output-port-output} to the port.
}

@defproc/nolink[(call-with-string-output-port [procedure (-> port? any)]) string?]{
The @var{procedure} value is called on a port. When it returns, @code{call-with-string-output-port}
returns a string containing the characters that were written to that port during the execution of
@var{procedure}.
}

@section{Revealed Ports and File Descriptors}

The material in this section and the following one is not critical for most applications. You may
safely skim or completely skip this section on a first reading.

Dealing with Unix file descriptors in a Scheme environment is difficult. In Unix, open files are
part of the process environment, and are referenced by small integers called
@emph{file descriptors}. Open file descriptors are the fundamental way I/O redirections are passed
to subprocesses, since file descriptors are preserved across fork's and exec's.

Scheme, on the other hand, uses ports for specifying I/O sources. Ports are garbage-collected Scheme
objects, not integers. Ports can be garbage collected; when a port is collected, it is also closed.
Because file descriptors are just integers, it's impossible to garbage collect them---you wouldn't
be able to close file descriptor 3 unless there were no 3's in the system, and you could further
prove that your program would never again compute a 3. This is difficult at best.

If a Scheme program only used Scheme ports, and never actually used file descriptors, this would not
be a problem. But Scheme code must descend to the file descriptor level in at least two
circumstances:

@itemlist[@item{when interfacing to foreign code}
          @item{when interfacing to a subprocess}]

This causes a problem. Suppose we have a Scheme port constructed on top of file descriptor 2. We
intend to fork off a program that will inherit this file descriptor. If we drop references to the
port, the garbage collector may prematurely close file 2 before we fork the subprocess. The
interface described below is intended to fix this and other problems arising from the mismatch
between ports and file descriptors.

The scsh kernel maintains a port table that maps a file descriptor to the scheme port allocated for
it (or, #f if there is no port allocated for this file descriptor). This is used to ensure that
there is at most one open port for each open file descriptor.

Conceptually, the port data structure for file ports has two fields besides the descriptor:
@var{revealed} and @var{closed?}. When a file port is closed with @code{(close port)}, the port's
file descriptor is closed, its entry in the port table is cleared, and the port's @var{closed?}
field is set to true.

When a file descriptor is closed with @code{(close fdes)}, any associated port is shifted to a new
file descriptor created with @code{(dup fdes)}. The port has its revealed count reset to zero (and
hence becomes eligible for closing on GC). See discussion below. To really put a stake through a
descriptor's heart without waiting for associated ports to be GC'd, you must say one of

@codeblock{(close (fdes->inport fdes))
           (close (fdes->output fdes))}

The @var{revealed} field is an aid to garbage collection. It is an integer semaphore. If it is zero,
the port's file descriptor can be closed when the port is collected. Essentially, the @var{revealed}
field reflects whether or not the port's file descriptor has escaped to the scheme user. If the
scheme user doesn't know what file descriptor is associated with a given port, then he can't
possibly retain an ``integer handle'' on the port after dropping pointers to the port itself, so the
garbage collector is free to close the file.

Ports allocated with @code{open-output-file} and @code{open-input-file} are unrevealed ports---i.e.,
@var{revealed} is initialised to 0. No one knows the port's file descriptor, so the file descriptor
can be closed when the port is collected.

The functions @code{fdes->output-port}, @code{fdes->input-port}, @code{port->fdes} are used to shift
back and forth between file descriptors and ports.  When @code{port->fdes} reveals a port's file
descriptor, it increments the port's @var{revealed} field.  When the user is through with the file
descriptor, he can call @code{(release-port-handle port)}, which decrements the count. The function
@code{(call/fdes fd/port proc)} automates this protocol. @code{call/fdes} uses @code{dynamic-wind}
to enforce the protocol. If @var{proc} throws out of the @code{call/fdes} application, the unwind
handler releases the descriptor handle; if the user subsequently tries to throw @emph{back} into
@var{proc}'s context, the wind handler raises an error. When the user maps a file descriptor to a
port with @code{fdes->outport} or @code{fdes->inport}, the port has its revealed field incremented.

Not all file descriptors are created by requests to make ports.  Some are inherited on process
invocation via
@hyperlink["http://www.FreeBSD.org/cgi/man.cgi?query=exec&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html"]{exec(2)}},
and are simply part of the global environment. Subprocesses may depend upon them, so if a port is
later allocated for these file descriptors, is should be considered as a revealed port. For example,
when the scheme shell's process starts up, it opens ports on file descriptors 0, 1, and 2 for the
initial values of @code{(current-input-port)}, @code{(current-output-port)}, and
@code{(error-output-port)}. These ports are initialised with @var{revealed} set to 1, so that stdin,
stdout, and stderr are not closed even if the user drops the port.

Unrevealed file ports have the nice property that they can be closed when all pointers to the port
are dropped. This can happen during gc, or at an @code{exec()}---since all memory is dropped at an
@code{exec()}. No one knows the file descriptor associated with the port, so the exec'd process
certainly can't refer to it.

This facility preserves the transparent close-on-collect property for file ports that are used in
straightforward ways, yet allows access to the underlying Unix substrate without interference from
the garbage collector. This is critical, since shell programming absolutely requires access to the
Unix file descriptors, as their numerical values are a critical part of the process interface.

A port's underlying file descriptor can be shifted around with @code{dup(2)} when convenient. That
is, the actual file descriptor on top of which a port is constructed can be shifted around
underneath the port by the scsh kernel when necessary.  This is important, because when the user is
setting up file descriptors prior to a @code{exec(2)}, he may explicitly use a file descriptor that
has already been allocated to some port. In this case, the scsh kernel just shifts the port's file
descriptor to some new location with @code{dup}, freeing up its old descriptor.  This prevents
errors from happening in the following scenario.  Suppose we have a file open on port @var{f}.  Now
we want to run a program that reads input on file 0, writes output to file 1, errors to file 2, and
logs execution information on file 3. We want to run this program with input from @code{f}. So we
write:

@codeblock{(run (/usr/shivers/bin/prog)
                (> 1 output.txt)
                (> 2 error.log)
                (> 3 trace.log)
                (= 0 ,f))}

Now, suppose by ill chance that, unbeknownst to us, when the operating system opened @code{f}'s
file, it allocated descriptor 3 for it. If we blindly redirect @code{trace.log} into file descriptor
3, we'll clobber @code{f}! However, the port-shuffling machinery saves us: when the @code{run} form
tries to dup @code{trace.log}'s file descriptor to 3, @code{dup} will notice that file descriptor 3
is already associated with an unrevealed port (i.e., @code{f}). So, it will first move @code{f} to
some other file descriptor. This keeps @code{f} alive and well so that it can subsequently be dup'd
into descriptor 0 for @code{prog}'s stdin.

The port-shifting machinery makes the following guarantee: a port is only moved when the underlying
file descriptor is closed, either by a @code{close()} or a @code{dup2()} operation. Otherwise a
port/file-descriptor association is stable.

Under normal circumstances, all this machinery just works behind the scenes to keep things
straightened out. The only time the user has to think about it is when he starts accessing file
descriptors from ports, which he should almost never have to do. If a user starts asking what file
descriptors have been allocated to what ports, he has to take responsibility for managing this
information.

@section{Port-Mapping Machinery}

The procedures provided in this section are almost never needed. You may safely skim or completely
skip this section on a first reading.

Here are the routines for manipulating ports in scsh. The important points to remember are:

@itemlist[@item{A file port is associated with an open file, not a particular file descriptor.}
          @item{The association between a file port and a particular file descriptor is never
                changed @emph{except} when the file descriptor is explicitly closed. ``Closing''
                includes being used as the target of a @code{dup2}, so the set of procedures below
                that close their targets are @code{close}, two-argument @code{dup}, and
                @code{move->fdes}. If the target file descriptor of one of these routines has an
                allocated port, the port will be shifted to another freshly-allocated file
                descriptor, and marked as unrevealed, thus preserving the port but freeing its old
                file descriptor.}]

These rules are what is necessary to ``make things work out'' with no surprises in the general case.

@deftogether[(@defproc/nolink[(fdes->inport [fd integer?]) port?]
              @defproc/nolink[(fdes->outport [fd integer?]) port?])]{
These guys return an input or output port respecitvely, backed by @var{fd}. The returned port has
its revealed count set to 1.
}

@; will need to export fdport? if we use it as a predicate here.
@defproc/nolink[(port->fdes [port fdport?]) integer?]{
Returns the file descriptor backing @var{port} and increments its revealed count by 1.
}

@defproc/nolink[(port-revealed [port fdport?]) (or/c integer? #f)]{
Return the port's revealed count if positive, otherwise #f.
}

@defproc/nolink[(release-port-handle [port fdport?]) undefined]{
Decrement @var{port}'s revealed count.
}

@defproc/nolink[(call/fdes [fd/port (or/c integer? fdport?)]
                                      [consumer (-> integer? any)]) any]{
Calls @var{consumer} on a file descriptor; takes care of revealed bookkeeping. If @var{fd/port} is a
file descriptor, this is just @code{(consumer fd/port)}. If @var{fd/port} is a port, calls
@var{consumer} on its underlying file descriptor. While @var{consumer} is running, the port's
revealed count is incremented.

When @code{call/fdes} is called with port argument, you are not allowed to throw into @var{consumer}
with a stored continuation, as that would violate the revealed-count bookkeeping.
}

@defproc/nolink[(move->fdes [fd/port (or/c integer? fdport?)] [target-fd integer?])
         (or/c integer? fdport?)]{
Maps fd -> fd and port ->port.

If @var{fd/port} is a file-descriptor not equal to @var{target-fd}, dup it to @var{target-fd} and
close it. Returns @var{target-fd}.

If @var{fd/port} is a port, it is shifted to @var{target-fd}, by duping its underlying
file-descriptor if necessary. @var{fd/port}'s original file descriptor is closed (if it was
different from @var{target-fd}). Returns the port. This operation resets @var{fd/port}'s revealed
count to 1.

In all cases when @var{fd/port} is actually shifted, if there is a port already using
@var{target-fd}, it is first relocated to some other file descriptor.
}

@section{Unix I/O}
@deftogether[(@defproc*[#:link-target? #f
                        ([(dup [fd/port (or/c integer? fdport?)]) fdport?]
                         [(dup [fd/port (or/c integer? fdport?)] [newfd integer?]) fdport?])]
              @defproc*[#:link-target? #f
                        ([(dup->inport [fd/port (or/c integer? fdport?)]) fdport?]
                         [(dup->inport [fd/port (or/c integer? fdport?)] [newfd integer?]) fdport?])]
              @defproc*[#:link-target? #f
                        ([(dup->outport [fd/port (or/c integer? fdport?)]) fdport?]
                         [(dup->outport [fd/port (or/c integer? fdport?)] [newfd integer?]) fdport?])]
              @defproc*[#:link-target? #f
                        ([(dup->fdes [fd/port (or/c integer? fdport?)]) integer?]
                         [(dup->fdes [fd/port (or/c integer? fdport?)] [newfd integer?]) integer?])])]{
These procedures provide the functionality of C's @code{dup()} and @code{dup2()}. The different
routines return different types of values: @code{dup->inport}, @code{dup->outport}, and
@code{dup->fdes} return input ports, output ports, and integer file descriptors, respectively.
@code{dup}'s return value depends on on the type of @var{fd/port}---it maps fd -> fd and port ->
port.

These procedures use the Unix @code{dup()} syscall to replicate the file descriptor or file port
@var{fd/port}. If a @var{newfd} file descriptor is given, it is used as the target of the dup
operation, i.e., the operation is a @code{dup2()}. In this case, procedures that return a port (such
as @code{dup->inport}) will return one with the revealed count set to one. For example,
@code{(dup (current-input-port) 5)} produces a new port with underlying file descriptor 5, whose
revealed count is 1. If @var{newfd} is not specified, then the operating system chooses the file
descriptor, and any returned port is marked as unrevealed.

If the @var{newfd} target is given, and some port is already using that file descriptor, the port is
first quietly shifted (with another @code{dup}) to some other file descriptor (zeroing its revealed
count).

Since scheme doesn't provide read/write ports, @code{dup->inport} and @code{dup->outport} can be
useful for getting an output version of an input port, or @emph{vice versa}.  For example, if
@code{p} is an input port open on a tty, and we would like to do output to that tty, we can simply
use @code{(dup->outport p)} to produce an equivalent output port for the tty. Be sure to open the
file with the @code{open/read+write} flag for this.
}

@defproc*[#:link-target? #f
          ([(seek [fd integer?] [offset integer?]) integer?]
           [(seek [fd integer?] [offset integer?] [whence integer?]) integer?])]{
Reposition the I/O cursor for a file descriptor. @var{whence} is one of @code{seek/set},
@code{seek/delta}, or @code{seek/end}, and defaults to @code{seek/set}. If @code{seek/set}, then
@var{offset} is an absolute index into the file; if @code{seek/delta}, then @var{offset} is a
relative offset from the current I/O cursor; if @code{seek/end}, then @var{offset} is a relative
offset from the end of file. Note that not all file descriptors are seekable; this is dependent on
the OS implementation. The return value is the resulting position of the I/O cursor in the I/O
stream.
}

@defproc/nolink[(tell [fd integer?]) integer?]{
Returns the position of the I/O cursor in the the I/O stream. Not all file descriptors support
cursor-reporting; this is dependent on the OS implementation.
}

@defproc/nolink[(open-file [fname string?] [options file-options?] [mode file-mode? (filemode read write)]) fdport?]{
@var{options} is a file-option? (ref). You must use exactly one of the options @code{read-only},
@code{write-only}, or @code{read-write}. The returned port is an input port if the @var{options}
permit it, otherwise an output port. scheme48/scsh do not have input/output ports, so it's one
or the other. You can hack simultaneous I/O on a file by opening it r/w, taking the result input
port, and duping it to an output port with @code{dup->outport}.
}

@deftogether[(@defproc/nolink[(open-input-file [fname string?] [options file-options? (file-options)]) port?]
              @defproc/nolink[(open-output-file [fname string?]
                                                [options file-options? (file-options create truncate)]
                                                [mode file-mode? (file-mode read write)]) port?])]{
These are equivalent to @code{open-file}, after first including @code{read-only} or
@code{write-only} options, respectively, in the @var{options} argument. The default values for
@var{options} make the procedures backwards-compatible with their unary RnRS definitions.
}

@defproc/nolink[(pipe) (values [rport fdport?] [wport fdport?])]{
Returns two ports @var{rport} and @var{wport}, the read and write endpoints respectively of a Unix
pipe.
}

@section{Buffered I/O}

@defproc/nolink[(force-output [fd/port (or/c integer? fdport?)]) undefined]{
This procedure does nothing when applied to an integer file descriptor. It flushes buffered output
when applied to a port, and raises a write-error exception on error. Returns no value.
}

@defproc/nolink[(flush-all-ports) undefined]{
This procedure flushes all open output ports with buffered data.
}

