#lang scribble/manual

@title{Input and Output}
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
@margin-note{
  In previous scsh releases, most standard RnRS I/O procedures used to accept integer file descriptors
  as valid read/write destinations. This feature has been deprecated due to limited use, multi-byte 
  encoding complications and conflicts with optimistic concurrency guarantees of Scheme 48's I/O system. 
  Consider @seclink["port-map-sec"]{constructing a port on the file descriptor} instead.
  }
All the standard RnRS I/O procedures are defined in terms of character-oriented I/O. As a result, 
the operations will necessarily be buffered (at least on per-character basis). This includes
@as-index[@code{read}], @as-index[@code{read-char}], @as-index[@code{peek-char}], 
@as-index[@code{char-ready?}], @as-index[@code{write}], @as-index[@code{write}], 
@as-index[@code{write-char}], @as-index[@code{display}], @as-index[@code{newline}] and 
@as-index[@code{format}]. @as-index[@code{write-string}] is also provided for convenience.

In a systems programming setting, however, byte-oriented i/o is likely desired. For this pupose, 
scsh offers the following variants of standard I/O procedures. See also @secref["byte-vectors-sec"].

@deftogether[(@defproc[(read-byte   [port fdport? (current-input-port)]) integer]
              @defproc[(peek-byte   [port fdport? (current-input-port)]) integer]
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

All scsh ports have an internal buffer which is used to support I/O operations.
Most importatnly, it facilitates character-based I/O by implicitly translating characters to and from 
their byte sequence representations based on the specified text codec. Additoinally, it enables 
transferring data to and from the OS in blocks which gives much better performance.

All new ports are block-buffered by default, but scsh provides a mechanism for configuring a 
buffering policy on fresh ports, before any I/O has taken place. 

@code{bufpol} is an enumeration describing valid buffering policies that can be set on a port. The 
syntax for @code{bufpol} and valid @var{policy-name}s are as follows:
@defform[(bufpol policy-name)]
@tabular[
  #:style 'boxed
  #:row-properties '((bottom-border top-border) ())
  #:sep @hspace[3]
    (list 
      (list ""  @bold{Policy name }   @bold{Description})
      (list ""  @code{block } "Default block buffering policy.")
      (list ""  @code{line  } "Line buffering policy, only available on output." )
      (list ""  @code{none  } "Unbuffered policy.")
  )]

@code{(bufpol block)}, also known as @as-index{@code{bufpol/block}}, is the default block buffering 
policy. On ouptut ports, it writes data into the internal buffer up until the limit capacity and
only then delivers data out to the system. On input ports, it performs read operations from the 
internal byte buffer and fills it to the limit whenever it gets empty or cannot decode a complete 
character. 

@code{(bufpol line)}, also known as @as-index{@code{bufpol/line}}, is line buffering policy. On
output ports, it acts exactly the same as a buffered port for all byte-oriented I/O. For
character-based I/O, it acts as a buffered port, except it also flushes automatically upon 
receiving a newline character. This buffering policy is likely preferred for TTY ports. This policy
is not available on input ports.

@code{(bufpol none)}, also known as @as-index{@code{bufpol/none}}, is an unbuffered I/O policy. 
On output ports, the data is delivered immediately after every write operation. Defining what 
unbuffered input means gets a bit tricky, but scsh understands it to mean that @emph{no unrequested
reading takes place}. Byte-oriented I/O happens directly, since we always know exactly how many 
bytes are requested. For character-based I/O, the internal buffer is filled one byte at a time 
until a character can be decoded. 

It can be useful to turn I/O buffering off in some cases, for example when an I/O stream is to be 
shared by multiple subprocesses. For this reason, scsh allocates an unbuffered port for file 
descriptor 0 at start-up time. Because shells frequently share stdin with subprocesses, if 
the shell does buffered reads, it might "steal" input intended for a subprocess. For this reason, 
all shells, including sh, csh, and scsh, read stdin unbuffered. Applications that can tolerate 
buffered input on stdin can reset @code{(current-input-port)} to block buffering for higher performance.

@defproc[(set-port-buffering [port port?] [policy bufpol?] [size integer? channel-buffer-size]) undefined]{
  This procedure allows the programmer to assign a particular I/O buffering @var{policy} to a 
  given @var{port}, and to choose the @var{size} of the associated buffer. It may only be used on new 
  ports, i.e., before I/O is performed on the ports.

  The @var{size} argument requests an I/O buffer of @var{size} bytes, which must be a non-negative
  number. If not given, a reasonable default is used. If given and 0, buffering policy is understood 
  to be @code{bufpol/none}.
}

@defproc[(force-output [fdport fdport?]) undefined]{
  This procedure flushes buffered output when applied to a output port and does nothing when applied
  to an input port. It raises a write-error exception on error. Returns no value.
}

@defproc[(flush-all-ports) undefined]{
  This procedure flushes all open output ports.
}

@section{String Ports}
  Scheme 48 has string ports, which are exposed to scsh users.

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

@section{Unix I/O}

Procedures in this section accept both fdports and integer file descriptors as valid targets. This 
allows users to manage file descriptors bypassing any Scheme overhead. See @secref["port-revealed-sec"] for
details.

@deftogether[(@defproc*[([(dup [fd/port (or integer? fdport?)]) fdport]
                         [(dup [fd/port (or integer? fdport?)] [newfd integer?]) fdport])]
              @defproc*[([(dup->inport [fd/port (or integer? fdport?)]) fdport]
                         [(dup->inport [fd/port (or integer? fdport?)] [newfd integer?]) fdport])]
              @defproc*[([(dup->outport [fd/port (or integer? fdport?)]) fdport]
                         [(dup->outport [fd/port (or integer? fdport?)] [newfd integer?]) fdport])]
              @defproc*[([(dup->fdes [fd/port (or integer? fdport?)]) integer]
                         [(dup->fdes [fd/port (or integer? fdport?)] [newfd integer?]) integer])])]{
  These procedures provide the functionality of C's @code{dup()} and @code{dup2()}. The different
  routines return different types of values: @code{dup->inport}, @code{dup->outport}, and
  @code{dup->fdes} return input ports, output ports, and integer file descriptors, respectively.
  @code{dup}'s return value depends on on the type of @var{fd/port}---it maps fd to fd and port to
  port.

  Each procedure has two variants. If @var{newfd} is not specified, this is a call to @code{dup()}.
  The operating system chooses the lowest unused file descriptor and @var{fd/port} is duped to it.  
  Any returned port is marked as unrevealed.
  
  If a @var{newfd} file descriptor is given, this is a call to @code{dup2()} and @var{newfd} is used 
  as the target. In this case, procedures that return a port will return one with the revealed count
  set to one. For example, @code{(dup (current-input-port) 5)} produces a new port with underlying 
  file descriptor 5, whose revealed count is 1. 

  If the @var{newfd} target is given, and some port is already using that file descriptor, that port is
  first quietly shifted (with another @code{dup()}) to some other file descriptor (zeroing its revealed
  count). Port shifting is discussed in detail in @secref["port-revealed-sec"].

  Since scheme doesn't provide read/write ports, @code{dup->inport} and @code{dup->outport} can be
  useful for getting an output version of an input port and vice versa.  For example, if @code{p} is
  an input port open on a tty, and we would like to do output to that tty, we can simply use 
  @code{(dup->outport p)} to produce an equivalent output port for the tty. Be sure to open the
  file with the @code{(file-flags read-write)} flag for this.
}

@defproc[(seek [fd/port (or integer? fdport?)] 
               [offset integer?] 
               [whence integer? seek/set]) integer]{
  Repositions the I/O cursor for a given @code{fd/port} with @code{lseek()}. 
    
  @var{whence} is a seek directive that specifies the behavior in terms of @var{offset}. If @var{whence}
  is not given, it defaults to @code{seek/set}. @var{whence} must be one of:
  @tabular[#:style 'boxed
           #:row-properties '((bottom-border top-border) '())
           #:sep @hspace[3]
    (list (list "" @bold{whence} @bold{Description})
      (list "" @as-index{@code{seek/set}} "offset is an absolute index into the file.")
      (list "" @as-index{@code{seek/delta}} "offset is a relative offset from the current I/O cursor.")
      (list "" @as-index{@code{seek/end}} "offset is a relative offset from the end of file."))]
  
  The return value is the resulting position of the I/O cursor in the I/O stream.  

  Since the procedure works on the file descriptor level, it might give unexpected results when 
  working with buffered ports; use unbuffered ports or raw file descriptors instead. Also, note that
  the cursor tracks bytes, not characters, so take caution when working with multibyte-encoded
  character data. Finally, not all file descriptors are seekable, this is dependent on the 
  OS implementation. 
}

@defproc[(tell [fd/port (or integer? fdport?)]) integer]{
  Returns the position of the I/O cursor in the the I/O stream for @code{fd/port}. Same warnings 
  apply as for @code{seek}.
}

@defproc[(pipe) (values [rport wport])]{
  Returns two fdports @var{rport} and @var{wport}, the read and write endpoints respectively of a Unix
  pipe. Both these ports are considered unrevealed and thus elegible for garbage collection.
}

@subsection{Opening Files and Changing File Status}

While scsh users are free to @code{open()}/@code{fcntl()} files with whatever flags they like, there 
are some details to be aware of to play nicely with Scheme 48. To avoid locking up the system,
all port I/O needs to be non-blocking. Scheme 48 handles input and output differently: read 
operations rely on @code{select()}/@code{poll()}, while write operations expect the output files to 
have @code{O_NONBLOCK} flag set. As a special case, stdout is left alone.

To make the common case work smoothly, scsh follows this rule and automaticaly sets @code{O_NONBLOCK} 
on all output files it opens. If your application relies of write blocking, you can disable this 
flag with @code{set-file-status-flags!}. 

@defproc[(open-file [pathname string?] 
                    [flags (or file-flags? integer?)] 
                    [mode (or file-mode? integer?) (file-mode read write)]) fdport]{
  @margin-note{
  You can hack simultaneous I/O on a file by opening it with @code{read-write} flag, taking the result
  output port, and duping it to an input port with @code{dup->inport}.
  }
  This procedure provides the functionality of @code{open()}. @var{pathname} is the intended pathname.
  The returned port is an input port if the @var{flags} contain @code{read-only} flag, otherwise it 
  is an output port. Since scheme does not have input/output ports, it has be one or the other.
  Also, note that if this is an output port, @code{non-blocking} flag will be applied automatically, 
  as described in the beginning of the section.

  @var{flags} is an enumerated set @code{file-flags} that represents file creation flags and 
  file status flags. See how to use @code{file-flags} in detail @seclink["file-flags-sec"]{here}.
  @var{flags} must contain exactly one access mode, i.e. @code{read-write}, @code{write-only} or
  @code{read-only}. 

  @margin-note{
  If you ever need a flag not defined in @code{file-flags}, you can always call @code{file-flags->integer}
  to convert the set to its binary representation and then @code{bitwise-ior} the desired bit.
  }
  For backwards compatibility, @var{flags} also accepts raw integers. To construct a valid @var{flags}
  value, use @code{bitwise-ior} with legacy flag bindings: @code{open/append}, @code{open/non-blocking}
  @code{open/async}, @code{open/fsync}, @code{open/read}, @code{open/write}, @code{open/read+write},
  @code{open/create}, @code{open/exclusive}, @code{open/no-control-tty} and @code{open/truncate}. We
  also provide @code{open/access-mask} for masking out the access mode.

  @var{mode} is an integer or an enumerated set @code{file-mode} that specifies the file mode bits to 
  be applied when a new file is created. If @var{flags} does not contain @code{create} flag, the 
  @var{mode} will be ignored. The default value is @code{(file-mode read write)} or #o666. See how 
  to use @code{file-mode} in detail @seclink["file-modes-sec"]{here}.
}


@deftogether[(@defproc[(open-input-file  [pathname string?] 
                                         [flags (or file-flags? integer?) (file-flags read-only)]) 
                                         fdport]
              @defproc[(open-output-file [pathname string?]
                                         [flags (or file-flags? integer?) (file-options create truncate non-blocking)]
                                         [mode file-mode? (file-mode read write)]) fdport])]{
  These are equivalent to @code{open-file}, after first including @code{read-only} or @code{write-only}
  flags, respectively. Also, note that if this is an output, @code{non-blocking} flag will be applied 
  as well, as described in the beginning of the section.

  The default values for @var{flags} make the procedures backwards-compatible with their unary RnRS 
  definitions.
}

@defproc[(open-fdes [pathname string?] 
                    [flags (or file-flags? integer?)] 
                    [mode (or file-mode? integer?) (file-mode read write)]) fdport]{
  Equivalent to @code{open-file}, but returns a file descriptor without constructing a port.
}

@deftogether[(@defproc[(close-on-exec?      [fd/port (or integer? fdport?)]) boolean]
              @defproc[(set-close-on-exec?! [fd/port (or integer? fdport?)] 
                                            [cloexec? boolean?]) undefined])]{
  These procedures provide the functionality of @code{fcntl()} with @code{F_GETFD}/@code{F_SETFD} 
  operations. Since, pretty much universally, the only flag associated with file descriptors is
  @code{FD_CLOEXEC}, we provide procedures just for this flag.

  These procedures should not be particularly useful to the programmer, as the scsh runtime already
  provides automatic control of the close-on-exec property. Unrevealed ports always have their file
  descriptors marked close-on-exec, as they can be closed when the scsh process execs a new program.
  Whenever the user reveals or unreveals a port's file descriptor, the runtime automatically sets or
  clears the flag for the programmer. Programmers that manipulate this flag should be aware of these
  extra, automatic operations. See @secref["port-revealed-sec"].

  @code{fdes-flags} and @code{set-fdes-flags} are given as legacy synonyms.
}

@deftogether[(@defproc[(file-status-flags      [fd/port (or integer? fdport?)]) file-flags]
              @defproc[(set-file-status-flags! [fd/port (or integer? fdport?)] 
                                               [flags   (or file-flags? integer?)]) undefined])]{
  These procedures provide the functionality of @code{fcntl()} with @code{F_GETFL}/@code{F_SETFL} 
  operations, i.e. allows reading and writing of an open file's status flags.

  The return value of @code{file-status-flags} and the @var{flags} argument are a @code{file-flags}
  enumerated set, as described @seclink["file-flags-sec"]{here}. For backwards compatibility, 
  @var{flags} also accepts raw integer values. See the discussion in @code{open-file}. 

  Note that this file-descriptor state is shared between file descriptors created by @code{dup()}.  
  If you create port @var{b} by applying dup to port @var{a}, and change @var{b}'s status flags, you 
  will also have changed @var{a}'s status flags. 

  @code{fdes-status} and @code{set-fdes-status} are given as legacy synonyms.
}

@subsection[#:tag "file-flags-sec"]{File Flags}

Scsh uses an enumerated set @code{file-flags} for representing file creation flags and file status 
flags. This gives us arbitrary-length set constructors, convenient set operations as well as a nice
abstraction for OS-specific flag values. The syntax for @code{file-flags} and valid @var{flag-name}s 
are as follows:
@defform[(file-flags flag-name ...+)]
@tabular[
  #:style 'boxed
  #:row-properties '((bottom-border top-border) () () () bottom-border () () () () () () bottom-border ())
  #:sep @hspace[1]
    (list 
      (list "" @bold{Flag name}   @bold{Description})
      (list "" "" @emph{Access mode flags, excatly one required for open()})
      (list ""  @code{read-write} "Open for reading and writing.")
      (list ""  @code{write-only} "Open for writing only.")
      (list ""  @code{read-only}  "Open for reading only.")
      (list "" "" @emph{File creation flags, only affect open()})
      (list ""  @code{cloexec}    "Automatically close file on execve.")
      (list ""  @code{create}     "Create file if it does not exist.")
      (list ""  @code{directory}  "Error if pathname is not a directory.")
      (list ""  @code{exclusive}  "Error if `create` flag is set and the file exists.")
      (list ""  @code{no-control-tty}  "If pathname refers to a terminal device, it will not become the process's controlling terminal. Default on freeBSD.")
      (list ""  @code{truncate}   "If file exists, it is truncated to zero length.")
      (list "" "" @emph{File status flags, used in open() and can be modified with fcntl()})
      (list ""  @code{append}     "Set file pointer to the end of the file before each write.")
      (list ""  @code{async}      "Enable the SIGIO signal to be sent to the process group when I/O is possible.")
      (list ""  @code{direct}     "Minimize or eliminate the cache effects of reading and writing.")
      (list ""  @code{data-sync}  "DSYNC, synchronized I/O *data* integrity completion.")
      (list ""  @code{file-sync}  "FSYNC/SYNC, synchronized I/O *file* integrity completion.")
      (list ""  @code{non-blocking}    "Non-blocking I/O.")
  )]

These would be examples of valid flag sets: 
@codeblock{
  (file-flags non-blocking)
  (file-flags write-only append create)}

@deftogether[(@defproc[(file-flags?    [x any]) boolean]
              @defproc[(file-flags->integer  [flags file-flags?]) integer]
              @defproc[(integer->file-flags  [raw-val integer?]) file-flags])]{
  @code{file-flags?} is a predicate for @code{file-flags}. @code{file-flags->integer} is a call into 
  runtime to encode @var{flags} and return an actual, OS-specific integer, representing the set. 
  @code{integer->file-flags} does the inverse, by decoding an integer value and constructing a 
  @code{file-flags} set. If an unrecognized bit is set, it is ignored.
}

@deftogether[(@defproc[(file-flags-on? [checked-flags file-flags?] 
                                       [target-flags file-flags?]) boolean]
              @defproc[(file-flags+ [flags0 file-flags?] 
                                    [flags1 file-flags?]) file-flags]
              @defproc[(file-flags- [flags0 file-flags?] 
                                    [flags1 file-flags?]) file-flags]
              @defproc[(mask-file-flags [flags file-flags?] 
                                        [mask file-flags?]) file-flags]
              @defproc[(file-access-mode [flags file-flags?]) file-flags])]{
  @code{file-flags-on?} checks if @var{target-flags} is a subset of @var{checked-flags}.
  @code{file-flags+} and @code{file-flags-} are set union and set difference, respecitvely.

  @code{mask-file-flags} is a set intersection, where mask is given in terms of @code{file-flags}. 
  For convenience, some masks are pre-defined:
  @tabular[#:style 'boxed
           #:row-properties '((bottom-border top-border) '())
           #:sep @hspace[3]
    (list (list "" @bold{Mask name} @bold{Description})
      (list "" @as-index{@code{access-mode-mask}}    "Only file access mode.")
      (list "" @as-index{@code{creation-flags-mask}} "Flags that are only valid on open().")
      (list "" @as-index{@code{status-flags-mask}}   "Flags that are only valid on fcntl() F_SETFL."))]

  @code{file-access-mode} is syntactic sugar, combining @code{mask-file-flags} and @code{access-mode-mask}.
}

@subsection[#:tag "file-modes-sec"]{File Permissions}

For file permissions, scsh supports both numeric representation (i.e. #o666) and an enumerated set 
@code{file-mode} (i.e. @code{(file-mode read write)}). Numeric notation is handy if you already know 
what you are doing, but the set notation is nice because it gives string names to permission 
configurations. 

@code{file-mode} has minimal overhead because under the hood it is not a proper enumerated set but a
macro with a simple record-wrapper around a numeric value. Still, thinking of it as a set might be 
pleasantly symmetric to @code{file-flags}.

The syntax for @code{file-mode} and valid @var{mode-names}s are as follows:
@defform[(file-mode mode-names ...+)]
@tabular[
  #:style 'boxed
  #:row-properties '((bottom-border top-border) ())
  #:sep @hspace[2]
    (list 
      (list ""  @bold{Permission  }   @bold{Value} @bold{Comment})
      (list ""  @code{set-uid     } "#o4000" "When set, makes an executable run with the privileges of the owner of the file.")
      (list ""  @code{set-gid     } "#o2000" "When set, makes an executable run with the privileges of the group of the file.")
      (list ""  @code{owner-read  } "#o0400" "")
      (list ""  @code{owner-write } "#o0200" "")
      (list ""  @code{owner-exec  } "#o0100" "")
      (list ""  @code{owner       } "#o0700" "All permissions set for owner.")
      (list ""  @code{group-read  } "#o0040" "")
      (list ""  @code{group-write } "#o0020" "")
      (list ""  @code{group-exec  } "#o0010" "")
      (list ""  @code{group       } "#o0070" "All permissions set for group.")
      (list ""  @code{other-read  } "#o0004" "")
      (list ""  @code{other-write } "#o0002" "")
      (list ""  @code{other-exec  } "#o0001" "")
      (list ""  @code{other       } "#o0007" "All permissions set for other.")
      (list ""  @code{read        } "#o0444" "Read permission set for all users.")
      (list ""  @code{write       } "#o0222" "Write permission set for all users.")
      (list ""  @code{exec        } "#o0111" "Execute permission set for all users.")
      (list ""  @code{all	        } "#o0777" "All permissions are set for all users.")
  )]

These would be examples of valid flag sets: 
@codeblock{
  (file-mode all)
  (file-mode read write)}

@deftogether[(@defproc[(file-mode?  [x any]) boolean]
              @defproc[(file-mode=? [mode0 file-mode?]
                                    [mode1 file-mode?]) boolean]
              @defproc[(file-mode->integer  [flags file-mode?]) integer]
              @defproc[(integer->file-mode  [raw-val integer?]) file-mode])]{
  @code{file-mode?} and @code{file-mode=?} are a predicate and comparator for @code{file-flags}, 
  respecitvely. 
  
  @code{file-mode->integer} reveals the numeric value of @code{file-mode} and 
  @code{integer->file-mode} converts @var{raw-val} into a @code{file-mode} set.
}

@deftogether[(@defproc[(file-mode+ [mode0 file-mode?]
                                   [mode1 file-mode?]) file-mode]
              @defproc[(file-mode- [mode0 file-mode?]
                                   [mode1 file-mode?]) file-mode])]{
  @code{file-mode+} and @code{file-mode-} are equivalent to set union and set difference, respecitvely.
}

@section[#:tag "port-revealed-sec"]{File Descriptors and Scheme Ports}
@margin-note{
  The material in this section is not critical for most applications. You may safely skim or 
  completely skip this section on a first reading.
}

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

@subsection{Port Table and Closing Ports}

The scsh kernel maintains a port table that maps a file descriptor to the scheme port allocated for
it. This is used to ensure that there is at most one open port for each open file descriptor. 
Additoinally, each entry is associated with a @var{revealed} count, which discussed in detail below.

@defproc[(close [fd/port (or fdport? integer?)]) boolean]{

  Scsh provides a generic @code{close} procedure that works on fdports as well as a raw file
  descriptors. The procedure returns true if it closed an open port or file descriptor. If the 
  port was already closed, it returns false; this is not an error.

  If @var{fd/port} is a port, the port's file descriptor is closed, its entry in the port table 
  is cleared. If the port was an output port, it's buffer is flushed before closing.

  If @var{fd/port} is a file descriptor, and it has a port allocated to it, the port is shifted to a
  new file descriptor created with @code{(dup fd/port)}. The port has its revealed count reset to 
  zero (and hence becomes eligible for closing on GC); see discussion below. The file requested
  file descriptor is then closed. This reflects the design criteria that ports are not associated
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

@subsection{Revealed Count and File Descriptor Conversions}

The @var{revealed} field is an aid to garbage collection. It is an integer semaphore. If it is zero,
the port's file descriptor can be closed when the port is collected. Essentially, the @var{revealed}
field reflects whether or not the port's file descriptor has escaped to the scheme user. If the
scheme user doesn't know what file descriptor is associated with a given port, then they can't
possibly retain an "integer handle" on the port after dropping pointers to the port itself, so the
garbage collector is free to close the file.

Ports allocated with @code{open-output-file} and @code{open-input-file} are unrevealed ports---i.e.,
@var{revealed} is initialised to 0. No one knows the port's file descriptor, so the file descriptor
can be closed when the port is collected. On the other hand, when the user maps a known file 
descriptor to a port with @code{fdes->outport} or @code{fdes->inport}, the port has its revealed 
field incremented.

Overall, the functions @code{fdes->outport}, @code{fdes->inport}, @code{port->fdes} are used to shift
back and forth between file descriptors and ports.  When @code{port->fdes} reveals a port's file
descriptor, it increments the port's @var{revealed} field.  When the user is through with the file
descriptor, they can call @code{(release-port-handle port)}, which decrements the count. The function
@code{(call/fdes fd/port proc)} automates this protocol. @code{call/fdes} uses @code{dynamic-wind}
to enforce the protocol. If @var{proc} throws out of the @code{call/fdes} application, the unwind
handler releases the descriptor handle; if the user subsequently tries to throw @emph{back} into
@var{proc}'s context, the wind handler raises an error. 

Not all file descriptors are created by requests to make ports.  Some are inherited on process
invocation via @code{exec()}, and are simply part of the global environment. Subprocesses may 
depend upon them, so if a port is later allocated for these file descriptors, is should be considered 
as a revealed port. For example, when the scheme shell's process starts up, it opens ports on file 
descriptors 0, 1, and 2 for the initial values of @code{(current-input-port)}, 
@code{(current-output-port)}, and @code{(error-output-port)}. These ports are initialised with
@var{revealed} set to 1, so that stdin, stdout, and stderr are not closed even if the user drops 
the port.

@subsection{Unrevealed Ports and Implicit Shifting}

Unrevealed file ports have the nice property that they can be closed when all pointers to the port
are dropped. This can happen during gc, or at an @code{exec()}---since all memory is dropped at an
@code{exec()}. No one knows the file descriptor associated with the port, so the exec'd process
certainly can't refer to it.

This facility preserves the transparent close-on-collect property for file ports that are used in
straightforward ways, yet allows access to the underlying Unix substrate without interference from
the garbage collector. This is critical, since shell programming absolutely requires access to the
Unix file descriptors, as their numerical values are a critical part of the process interface.

A port's underlying file descriptor can be shifted around with @code{dup()} when convenient. That
is, the actual file descriptor on top of which a port is constructed can be shifted around
underneath the port by the scsh kernel when necessary.  This is important, because when the user is
setting up file descriptors prior to a @code{exec()}, they may explicitly use a file descriptor that
has already been allocated to some port. In this case, the scsh kernel just shifts the port's file
descriptor to some new location with @code{dup}, freeing up its old descriptor.  

This prevents errors from happening in the following scenario. Suppose we have a file open on port
@var{f}. Now we want to run a program that reads input on file 0, writes output to file 1, errors to 
file 2, and logs execution information on file 3. We want to run this program with input from @code{f}. 
So we write:

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

@section[#:tag "port-map-sec"]{Port-Mapping Interface}

The port-mapping machinery makes the following guarantee: the association between a file port and a 
particular file descriptor is never changed @emph{except} when the file descriptor is explicitly closed.
This can happen as a result of @code{close}, two-argument @code{dup}, and @code{move->fdes}, 
each of which rely on @code{dup2()} syscall. Otherwise a port/file-descriptor association is stable.

Under normal circumstances, all this machinery just works behind the scenes to keep things
straightened out. The only time the user has to think about it is when they start accessing file
descriptors from ports, which they should almost never have to do. If a user starts asking what file
descriptors have been allocated to what ports, they have to take responsibility for managing this
information.

@deftogether[(@defproc[(fdes->inport [fd integer?]) fdport]
              @defproc[(fdes->outport [fd integer?]) fdport])]{
  These procedures return an input or output port respecitvely, backed by @var{fd}. 
  The returned port has its revealed count set to 1.
}

@defproc[(port->fdes [port fdport?]) integer]{
  Returns the file descriptor backing @var{port} and increments its revealed count by 1.
}

@defproc[(port-revealed [port fdport?]) (values integer or #f)]{
  Return the port's revealed count if positive, otherwise #f.
}

@defproc[(release-port-handle [port fdport?]) undefined]{
  Decrement @var{port}'s revealed count.
}

@defproc[(call/fdes [fd/port (or integer? fdport?)]
                    [consumer (-> integer? any)]) any]{
  Calls @var{consumer} on a file descriptor; takes care of revealed bookkeeping. If @var{fd/port} is a
  file descriptor, this is just @code{(consumer fd/port)}. If @var{fd/port} is a port, calls
  @var{consumer} on its underlying file descriptor. While @var{consumer} is running, the port's
  revealed count is incremented.

  When @code{call/fdes} is called with port argument, you are not allowed to throw into @var{consumer}
  with a stored continuation, as that would violate the revealed-count bookkeeping.
}

@defproc[(move->fdes [fd/port (or integer? fdport?)] [target-fd integer?])
         (values fdport or integer)]{
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

