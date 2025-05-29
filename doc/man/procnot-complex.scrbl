#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title{More Complex Process Operations}
The procedures and special forms in @secref["using-extended-process-forms-in-scheme"] provide for the
common case, where the programmer is only interested in the output of the process. These special
forms and procedures provide more complicated facilities for manipulating processes.

@section{Pids and Ports Together}

@deftogether[(@defform/nolink[(run/port+proc epf)]
              @defproc/nolink[(run/port+proc* [thunk (-> any/c)]) (values [port port?] [proc process?])])]{
This special form and its analogous procedure can be used if the programmer also wishes access to
the process' pid, exit status, or other information. They both fork off a subprocess, returning two
values: a port open on the process' stdout (and current output port), and the subprocess's process
object. A process object encapsulates the subprocess' process id and exit code; it is the value
passed to the @code{wait} system call.
@; make the description of process objects a reference

For example, to uncompress a tech report, reading the uncompressed data into scsh, and also be able
to track the exit status of the decompression process, use the following:

@codeblock{(receive (port child) (run/port+proc (zcat tr91-145.tex.Z))
             (let* ((paper (port->string port))
                    (status (wait child)))
               ... use paper, status, and child here ...))}

Note that you must @emph{first} do the @code{port->string} and @emph{then} do the wait---the other
way around may lock up when the zcat fills up its output pipe buffer.
}

@section{Multiple Stream Capture}
Occasionally, the programmer may want to capture multiple distinct output streams from a process.
For instance, he may wish to read the stdout and stderr streams into two distinct strings. This is
accomplished with the @code{run/collecting} form and its analogous procedure,
@code{run/collecting*}.

@deftogether[(@defform/nolink[(run/collecting fds ...+ epf)]
              @defproc/nolink[(run/collecting* [fds (listof integer?)]
                                                          [thunk (-> any/c)])
                       (values [status integer?] [port port?] ...+)])]
These guys run processes that produce multiple output streams and return ports open on these
streams. To avoid issues of deadlock, @code{run/collecting} doesn't use pipes. Instead, it first
runs the process with output to temp files, then returns ports open on the temp files. For example,

@codeblock{(run/collecting (1 2) (ls))}

runs @code{ls} with stdout (fd 1) and stderr (fd 2) redirected to temporary files. When the
@code{ls} is done, @code{run/collecting} returns three values: the @code{ls} process' exit status,
and two ports open on the temporary files. The files are deleted before @code{run/collecting}
returns, so when the ports are closed, they vanish. The @code{fds} list of file descriptors is
implicitly backquoted by the special-form version.

For example, if Kaiming has his mailbox protected, then

@codeblock{(receive (status out err)
                    (run/collecting (1 2) (cat /usr/kmshea/mbox))
             (list status (port->string out) (port->string err)))}

might produce the list

@codeblock{(256 "" "cat: /usr/kmshea/mbox: Permission denied")}

What is the deadlock hazard that causes @code{run/collecting} to use temp files? Processes with
multiple output streams can lock up if they use pipes to communicate with Scheme I/O readers. For
example, suppose some Unix program @code{myprog} does the following:

@itemlist[#:style 'ordered
          @item{First, outputs a single ``@exec{(}'' to stderr.}
          @item{Then, outputs a megabyte of data to stdout.}
          @item{Finally, outputs a single ``@exec{)}'' to stderr, and exits.}]

Our scsh programmer decides to run @code{myprog} with stdout and stderr redirect
@emph{via Unix pipes} to the ports @code{port1} and code{port2}, respectively. He gets into trouble
when he subsequently says @code{(read port2)}. The Scheme @code{read} routine reads the open paren,
and then hangs in a
@hyperlink["http://www.FreeBSD.org/cgi/man.cgi?query=read&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html"]{read()}}
system call trying to read a matching close paren. But before @code{myprog} sends the close paren
down the stderr pipe, it first tries to write a megabyte of data to the stdout pipe. However, Scheme
is not reading that pipe---it's stuck waiting for input on stderr. So the stdout pipe quickly fills
up, and @code{myprog} hangs, waiting for the pipe to drain. The @code{myprog} child is stuck in a
stdout/@code{port1} write; the Scheme parent is stuck in a stderr/@code{port2} read. Deadlock.

@; ^might not be a problem in modern s48. doesn't use blocking read? research.

Here's a concrete example that does exactly the above:

@codeblock{(receive (status port1 port2)
                    (run/collecting (1 2) 
             (begin
               ;; Write an open paren to stderr.
               (run (echo "(") (= 1 2))
               ;; Copy a lot of stuff to stdout.
               (run (cat /usr/dict/words))
               ;; Write a close paren to stderr.
               (run (echo ")") (= 1 2))))

   ;; OK. Here, I have a port PORT1 built over a pipe
   ;; connected to the BEGIN subproc's stdout, and
   ;; PORT2 built over a pipe connected to the BEGIN
   ;; subproc's stderr.
   (read port2) ; Should return the empty list.
   (port->string port1)) ; Should return a big string.
}

In order to avoid this problem, @code{run/collecting} and @code{run/collecting*} first run the child
process to completion, buffering all the output streams in temp files (using the
@code{temp-file-channel} procedure). When the child process exits, ports open on the buffered output
are returned. This approach has two disadvantages over using pipes:

@; link to temp-file-channel definition

@itemlist[@item{The total output from the child output is temporarily written to the disk before
                returning from @code{run/collecting}. If this output is some large intermediate
                result, the disk could fill up.}
          @item{The child producer and Scheme consumer are serialised; there is no concurrency
                overlap in their execution.}]

However, it remains a simple solution that avoids deadlock.  More sophisticated solutions can easily
be programmed up as needed---@code{run/collecting*} itself is only 12 lines of simple code.

See @code{temp-file-channel} for more information on creating temp files as communication channels.