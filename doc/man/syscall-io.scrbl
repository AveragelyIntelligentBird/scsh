#lang scribble/manual

@title{Input and Output}
@margin-note{
  In previous scsh releases, most I/O procedures used to accept integer file descriptors as valid 
  read/write destinations. This feature has been deprecated due to limited use, multi-byte encoding
  complications and conflicts with optimistic concurrency guarantees of Scheme 48's I/O system. 
  Consider @seclink["port-map-sec"]{constructing a port on the file descriptor} instead.
  }
Scsh utilizes extended Scheme 48 ports as input and output devices for all its I/O procedures. 
This means that every port is associated with:
@itemlist[
  @item{Separate handlers for reading/writing @emph{bytes, chars} and @emph{blocks of bytes}. In a 
        multi-byte character encoding world, the distinction is important.}
  @item{@seclink["text-codecs-sec"]{Text codec}, allowing seamless character encoding and decoding 
        when using character i/o.}
  @item{@seclink["buffering-sec"]{Buffering policy}, which can be configured before any i/o has taken 
        place.}
  @item{An internal buffer, which is used for buffered i/o and character endocing/decoding. Unbuffered 
        port operatoins bypass the the port when possible.}
  @item{If the port is opened on a file, it is also associated with a 
        @seclink["port-revealed-sec"]{file descriptor}. Note that scsh uses an alternative 
        implementation for OS channel ports, so Scheme 48 functions cannot be used on scsh @code{fdports}.}
  ]

@section{RnRS I/O Procedures and Extentions}

All the standard RnRS I/O procedures are defined in terms of character-oriented I/O. As a result, 
the operations will necessarily be buffered (at least on per-character basis). This includes
@as-index[@code{read}], @as-index[@code{read-char}], @as-index[@code{peek-char}], 
@as-index[@code{char-ready?}], @as-index[@code{write}], @as-index[@code{write}], 
@as-index[@code{write-char}], @as-index[@code{display}], @as-index[@code{newline}] and 
@as-index[@code{format}]. @as-index[@code{write-string}] is also provided for convenience.

In a systems programming setting, however, byte-oriented i/o is likely desired. For this pupose, 
scsh offers the following variants of standard I/O procedures. See also @secref["byte-vectors-sec"].

@deftogether[(@defproc[(read-byte   [port fdport? (current-input-port)]) byte]
              @defproc[(peek-byte   [port fdport? (current-input-port)]) byte]
              @defproc[(byte-ready? [port fdport? (current-input-port)]) boolean]
              @defproc[(write-byte  [byte byte?] [port fdport? (current-output-port)]) unspecific])]{
These procedures allow for single-byte I/O on a given @var{port}, which defaults to standard i/o streams.
@var{read-byte} reads a single byte and @var{write-byte} writes a single byte. @var{byte-ready?} 
tells if a byte is available to be read.

@var{peek-byte} returns the next byte in the local buffer without moving the cursor. Note that this
does not work on unbuffered ports, since Unix syscalls cannot peek head.}

@deftogether[(@defproc[(read-block [buffer byte-vector?] [start integer?] [count integer?] 
                        [port fdport? (current-input-port)]
                        [wait? boolean? #t]) integer]
              @defproc[(write-block [buffer byte-vector?] [start integer?] [count integer?] 
                        [port fdport? (current-output-port)]) unspecific])]{
@margin-note{
  @code{read-block} approximates the behavior of @code{read-string/partial} (defined in previous 
  scsh releases) adapted for multi-byte encoded strings. @code{write-string/partial} does not 
  apply to this release at all since all output i/o is non-blocking by default.
  }
These procedures allow for byte-oriented block I/O on a given @var{port}, which defaults to standard 
i/o streams. @var{read-block} reads a block of bytes from @var{port} into the given @var{buffer}, 
directed by @var{start} index and @var{count} of bytes desired. If @var{wait?} is true, the thread
will wait for input, otherwise, the procedure reaturns immediately. The returned integer is the number
of bytes read. 

@var{write-block} writes a block of bytes from @var{buffer} to @var{port}, directed by @var{start} 
index and @var{count} of bytes desired. The write will always complete.}

@section{Standard I/O Ports}

In addition to the standard @as-index[@code{current-input-port}] and
@as-index[@code{current-output-port}], scsh also defines a binding for a port currently used 
for error messages---the scsh equivalent of stderr.

@defproc[(error-output-port) fdport]

@margin-note{
  This means that there is no need to synchronize Unix' standard I/O file descriptors and 
  Scheme's current I/O ports. You can assume that at scsh top level the stdports are always bound 
  to stdio.
}
The bindings for standard I/O ports are fluids, which are set at startup of the system. 
The bindings cannot be mutated with @code{set!} as it was in earlier releases, so instead scsh 
provides procedures for temporarily installing alternative ports to these bindings. 


@deftogether[(@defproc[(with-current-input-port* [port fdport?] [thunk (-> any)]) any]
              @defproc[(with-current-output-port* [port fdport?] [thunk (-> any)]) any]
              @defproc[(with-error-output-port* [port fdport?] [thunk (-> any)]) any]
              @defproc[(with-current-ports* [input fdport?] [output fdport?] [error fdport?] 
                                            [thunk (-> any)]) any])]{
  These procedures install @var{port} as the current input, current output, and error output port,
  respectively, for the duration of a call to @var{thunk} and return @var{thunk}'s value(s).

  @var{with-current-ports} is equivalent to calling the previous three procedures nested one within 
  another.
}

@deftogether[(@defform[(with-current-input-port port body ...+)]
              @defform[(with-current-output-port port body ...+)]
              @defform[(with-error-output-port port body ...+)]
              @defform[(with-current-ports input output error body ...+)])]{
  These special forms are simply syntactic sugar for the @code{with-current-input-port*} procedure and
  friends.
}

@defproc[(stdports->stdio) unspecific]{
  Legacy procedure kept for convenience, although a lot less useful now. Synchronises Unix' standard 
  I/O file descriptors and Scheme's current I/O ports. Causes the standard I/O file descriptors 
  (0, 1, and 2) take their values from the current I/O ports. It is exactly equivalent to the 
  series of redirections:

  @codeblock{(dup (current-input-port)  0)
            (dup (current-output-port) 1)
            (dup (error-output-port)   2)}

  We use @code{dup} and not @code{move->fdes} because the current output port and error port might 
  be the same port.
}

@section[#:tag "buffering-sec"]{Buffered I/O}

ports are internally buffered to facilitate character encoding/decoding, but if the port is 
configured to be unbuf, the internal buffer is bypassed when possible (i.e. non-char stuff).

Scheme 48 ports use buffered I/O: data is transferred to or from the OS in blocks. 
Scsh provides control of this mechanism: the programmer may force saved-up output data to be 
transferred to the OS when he chooses, and may also choose which I/O buffering policy to employ 
for a given port (or turn buffering off completely).

It can be useful to turn I/O buffering off in some cases, for example when an I/O stream is to be 
shared by multiple subprocesses. For this reason, scsh allocates an unbuffered port for file 
descriptor 0 at start-up time. Because shells frequently share stdin with subprocesses, if 
the shell does buffered reads, it might ``steal'' input intended for a subprocess. For this reason, 
all shells, including sh, csh, and scsh, read stdin unbuffered. Applications that can tolerate 
buffered input on stdin can reset (current-input-port) to block buffering for higher performance.

{Note To support peek-char a Scheme implementation has to maintain a buffer for all input ports. 
In scsh, for ``unbuffered'' input ports the buffer size is one. As you cannot request less then one 
character there is no unrequested reading so this can still be called ``unbuffered input''.}

(set-port-buffering! port policy [size])     --->     undefined         (procedure) 

@code{set-port-buffering} is given as a legacy synonym.

This procedure allows the programmer to assign a particular I/O buffering policy to a port, 
and to choose the size of the associated buffer. It may only be used on new ports, i.e., 
before I/O is performed on the port. There are three buffering policies that may be chosen:

        bufpol/block 	General block buffering (general default)
        bufpol/line 	Line buffering (tty default)
        bufpol/none 	Direct I/O -- no buffering4

The line buffering policy flushes output whenever a newline is output; whenever the buffer is 
full; or whenever an input is read from stdin. Line buffering is the default for ports open on 
terminal devices.

The size argument requests an I/O buffer of size bytes. For output ports, size must be non-negative, 
for input ports size must be positve. If not given, a reasonable default is used. For output ports, 
if given and zero, buffering is turned off (i.e., size = 0 for any policy is equivalent to policy
 = bufpol/none). For input ports, setting the size to one corresponds to unbuffered input as defined 
 above. If given, size must be zero respectively one for bufpol/none.


@defproc[(force-output [fd/port (or/c integer? fdport?)]) undefined]{
This procedure does nothing when applied to an integer file descriptor. It flushes buffered output
when applied to a port, and raises a write-error exception on error. Returns no value.
}

@defproc[(flush-all-ports) undefined]{
This procedure flushes all open output ports with buffered data.
}

@section{String ports}
Scheme 48 has string ports, which you can use. Scsh has not committed to the particular interface or
names that scheme48 uses, so be warned that the interface described herein may be liable to change.

@defproc[(make-string-input-port [string string?]) port?]{
Returns a port that reads characters from the supplied string.
}

@deftogether[(@defproc[(make-string-output-port) port?]
              @defproc[(string-output-port-output [port port?]) string?])]{
A string output port is a port that collects the characters given to it into a string. The
accumulated string is retrieved by applying @code{string-output-port-output} to the port.
}

@defproc[(call-with-string-output-port [procedure (-> port? any)]) string?]{
The @var{procedure} value is called on a port. When it returns, @code{call-with-string-output-port}
returns a string containing the characters that were written to that port during the execution of
@var{procedure}.
}

@section[#:tag "port-revealed-sec"]{Revealed Ports and File Descriptors}

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

@section[#:tag "port-map-sec"]{Port-Mapping Machinery}

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

@deftogether[(@defproc[(fdes->inport [fd integer?]) port?]
              @defproc[(fdes->outport [fd integer?]) port?])]{
These guys return an input or output port respecitvely, backed by @var{fd}. The returned port has
its revealed count set to 1.
}

@; will need to export fdport? if we use it as a predicate here.
@defproc[(port->fdes [port fdport?]) integer?]{
Returns the file descriptor backing @var{port} and increments its revealed count by 1.
}

@defproc[(port-revealed [port fdport?]) (or/c integer? #f)]{
Return the port's revealed count if positive, otherwise #f.
}

@defproc[(release-port-handle [port fdport?]) undefined]{
Decrement @var{port}'s revealed count.
}

@defproc[(call/fdes [fd/port (or/c integer? fdport?)]
                                      [consumer (-> integer? any)]) any]{
Calls @var{consumer} on a file descriptor; takes care of revealed bookkeeping. If @var{fd/port} is a
file descriptor, this is just @code{(consumer fd/port)}. If @var{fd/port} is a port, calls
@var{consumer} on its underlying file descriptor. While @var{consumer} is running, the port's
revealed count is incremented.

When @code{call/fdes} is called with port argument, you are not allowed to throw into @var{consumer}
with a stored continuation, as that would violate the revealed-count bookkeeping.
}

@defproc[(move->fdes [fd/port (or/c integer? fdport?)] [target-fd integer?])
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

@defproc[(close [fd/port (or/c fdport? integer?)]) boolean]{
  scsh provides a generic @code{close} procedure that works on any @code{fdport} as well as a raw
  file descriptor. The procedure returns true if it closed an open port or file descriptor. If the 
  port was already closed, it returns false; this is not an error.

  If @var{fd/port} is a file descriptor, and it has a port allocated to it, the port is shifted to a
  new file descriptor created with @code{(dup fd/port)} before closing @code{fd/port}. The port then
  has its revealed count set to zero.  This reflects the design criteria that ports are not associated
  with file descriptors, but with the streams they denote.

  To close a file descriptor, and any associated port it might have, you must instead say one of
  (as appropriate):
  @codeblock{
    (close (fdes->inport  fd))
    (close (fdes->outport fd))}
}

@defproc[(close-after [port port?] [consumer (-> port? any)]) any]{
  Returns @code{(consumer port)}, but closes the port on return. No dynamic-wind magic.
}

@section{Unix I/O}
@deftogether[(@defproc*[([(dup [fd/port (or/c integer? fdport?)]) fdport?]
                         [(dup [fd/port (or/c integer? fdport?)] [newfd integer?]) fdport?])]
              @defproc*[([(dup->inport [fd/port (or/c integer? fdport?)]) fdport?]
                         [(dup->inport [fd/port (or/c integer? fdport?)] [newfd integer?]) fdport?])]
              @defproc*[([(dup->outport [fd/port (or/c integer? fdport?)]) fdport?]
                         [(dup->outport [fd/port (or/c integer? fdport?)] [newfd integer?]) fdport?])]
              @defproc*[([(dup->fdes [fd/port (or/c integer? fdport?)]) integer?]
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

@tabular[#:row-properties (list 'bottom-border 'top)
         #:sep @hspace[3]
            (list (list "Value"       "Behavior")
                  (list @code{#f}      @nested{signal an error (default)})
                  (list @code{'query}  @nested{prompt the user})
                  (list @emph{other}   @nested{delete the old object (with @code{delete-file} or
                                                @code{delete-directory} as appropriate) before 
                                                creating the new object}))]




@defproc[(open-file [fname string?] [options file-options?] [mode file-mode? (filemode read write)]) fdport?]{
@var{options} is a file-option? (ref). You must use exactly one of the options @code{read-only},
@code{write-only}, or @code{read-write}. The returned port is an input port if the @var{options}
permit it, otherwise an output port. scheme48/scsh do not have input/output ports, so it's one
or the other. You can hack simultaneous I/O on a file by opening it r/w, taking the result input
port, and duping it to an output port with @code{dup->outport}.
}

NON blocking output by default, select/poll on input

if you want it to block, you gotta do it explicitly

@deftogether[(@defproc[(open-input-file [fname string?] [options file-options? (file-options)]) port?]
              @defproc[(open-output-file [fname string?]
                                         [options file-options? (file-options create truncate)]
                                         [mode file-mode? (file-mode read write)]) port?])]{
These are equivalent to @code{open-file}, after first including @code{read-only} or
@code{write-only} options, respectively, in the @var{options} argument. The default values for
@var{options} make the procedures backwards-compatible with their unary RnRS definitions.
}

@defproc[(open-fdes [fname string?]
                    [options file-options? (file-options create truncate)]
                    [mode file-mode? (file-mode read write)]) port?]{
  ADD!!
}

@deftogether[(@defproc[(close-on-exec?      [fd/port (or/c integer? fdport?)]) boolean]
              @defproc[(set-close-on-exec?! [fd/port (or/c integer? fdport?)] 
                                             [cloexec? boolean?]) undefined])]{
  @code{fdes-flags} and @code{set-fdes-flags} are given as legacy synonyms.
}


@deftogether[(@defproc[(file-status-flags      [fd/port (or/c integer? fdport?)]) file-flags]
              @defproc[(set-file-status-flags! [fd/port (or/c integer? fdport?)] 
                                                [flags file-flags?]) undefined])]{
  @code{fdes-status} and @code{set-fdes-status} are given as legacy synonyms.
}

@defproc[(seek [fd integer?] [offset integer?] [whence integer? seek/set]) integer?]{
Reposition the I/O cursor for a file descriptor. @var{whence} is one of @code{seek/set},
@code{seek/delta}, or @code{seek/end}, and defaults to @code{seek/set}. If @code{seek/set}, then
@var{offset} is an absolute index into the file; if @code{seek/delta}, then @var{offset} is a
relative offset from the current I/O cursor; if @code{seek/end}, then @var{offset} is a relative
offset from the end of file. Note that not all file descriptors are seekable; this is dependent on
the OS implementation. The return value is the resulting position of the I/O cursor in the I/O
stream.

<this is in bytes, not characters, so take caution with mutlibyte encodings>

}

@defproc[(tell [fd integer?]) integer?]{
Returns the position of the I/O cursor in the the I/O stream. Not all file descriptors support
cursor-reporting; this is dependent on the OS implementation.
}

@defproc[(pipe) (values [rport fdport?] [wport fdport?])]{
Returns two ports @var{rport} and @var{wport}, the read and write endpoints respectively of a Unix
pipe.
}

