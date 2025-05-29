#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title[#:tag "using-extended-process-forms-in-scheme"]{Using Extended Process Forms in Scheme}
Process forms and extended process forms are @emph{not} Scheme. They are a different notation for
expressing computation that, like Scheme, is based upon s-expressions. Extended process forms are
used in Scheme programs by embedding them inside special Scheme forms. There are three basic Scheme
forms that use extended process forms: @code{exec-epf}, @code{&}, and @code{run}.

@codeblock{(exec-epf . epf) ; => no return value (syntax)
           (& . epf)        ; => proc (syntax)
           (run . epf)      ; => status (syntax)}

The @code{(exec-epf . epf)} form nukes the current process: it establishes the I/O redirections and
then overlays the current process with the requested computation.

The @code{(& . epf)} form is similar, except that the process is forked off in background. The form
returns the subprocess' process object.

The @code{(run . epf)} form runs the process in foreground: after forking off the computation, it
waits for the subprocess to exit, and returns its exit status.

These special forms are macros that expand into the equivalent series of system calls. The
definition of the @code{exec-epf} macro is non-trivial, as it produces the code to handle I/O
redirections and set up pipelines.

However, the definitions of the @code{&} and @code{run} macros are very simple:

@tabular[#:sep @hspace[1]
         (list (list @code{(& . epf)} @code{(fork (lambda () (exec-epf . epf)))})
               (list @code{(run . epf)} @code{(wait (& . epf))}))]

@section{Procedures and Special Forms}
It is a general design principle in scsh that all functionality made available through special
syntax is also available in a straightforward procedural form. So there are procedural equivalents
for all of the process notation. In this way, the programmer is not restricted by the particular
details of the syntax.

Here are some of the syntax/procedure equivalents:

@tabular[#:sep @hspace[1]
         (list (list @exec{|} @code{fork/pipe})
               (list @exec{|+} @code{fork/pipe+})
               (list @code{exec-epf} @code{exec-path})
               (list "redirection" @nested{@code{open}, @code{dup}})
               (list @code{&} @code{fork})
               (list @code{run} @nested{@code{exec} + @code{fork}}))]

Having a solid procedural foundation also allows for general notational experimentation using
Scheme's macros. For example, the programmer can build his own pipeline notation on top of the
@code{fork} and @code{fork/pipe} procedures.

@;@ref[chapt:syscalls] fix this ref
gives the full story on all the procedures in the syscall library.

@section{Interfacing Process Output to Scheme}
There is a family of procedures and special forms that can be used to capture the output of
processes as Scheme data. These forms all fork off subprocesses, collecting the process' output to
stdout in some form or another. The subprocess runs with file descriptor 1 and the current output
port bound to a pipe. Furthermore, each of these forms is a simple expansion into calls to analogous
procedures. For example, @code{(run/port . epf)} expands into
@code{(run/port* (lambda () (exec-epf . epf)))}.

@deftogether[(@defform/nolink[(run/port epf)]
              @defproc/nolink[(run/port* [thunk (-> any/c)]) port?])]{
Returns a port open on process's stdout. Returns immediately after forking child process.
}

@deftogether[(@defform/nolink[(run/file epf)]
              @defproc/nolink[(run/file* [thunk (-> any/c)]) string?])]{
Returns the name of a temp file containing the process's output. Returns when the process exits.
}

@deftogether[(@defform/nolink[(run/string epf)]
              @defproc/nolink[(run/string* [thunk (-> any/c)]) string?])]{
Returns a string containing the process' output. Returns when an eof is read.
}

@deftogether[(@defform/nolink[(run/strings epf)]
              @defproc/nolink[(run/strings* [thunk (-> any/c)]) (listof string?)])]{
Splits process' output into a list of newline-delimited strings. The delimiting newlines are not
part of the returned strings. Returns when an eof is read.
}

@deftogether[(@defform/nolink[(run/sexp epf)]
              @defproc/nolink[(run/sexp* [thunk (-> any/c)]) any/c])]{
Returns a single object from process' stdout with @code{read}. Returns as soon as the read
completes.
}

@deftogether[(@defform/nolink[(run/sexps epf)]
              @defproc/nolink[(run/sexps* [thunk (-> any/c)]) (listof any/c)])]{
Repeatedly reads objects from process' stdout with @code{read}. Returns accumulated list upon eof.
}

@subsection{Parsing Input from Ports}
The following procedures are also of utility for generally parsing input streams in scsh:

@defproc/nolink[(port->string [port port?]) string?]{
Reads the port until eof, then returns the accumulated string.
}

@defproc/nolink[(port->sexp-list [port port?]) (listof any/c)]{
Repeatedly reads data from the port until eof, then returns the accumulated list of items.
}

@defproc/nolink[(port->string-list [port port?]) (listof string?)]{
Repeatedly reads newline-terminated strings from the port until eof, then returns the accumulated
list of strings. The delimiting newlines are not part of the returned strings.
}

@defproc/nolink[(port->list [reader (-> port? any/c)] [port port?]) (listof any/c)]{
Generalises @code{port->sexp-list} and @code{port->string-list}. Uses @code{reader} to repeatedly
read objects from a port and accumulates these objects into a list, which is returned upon eof.
@code{port->sexp-list} and @code{port->string-list} are trivial to define, being merely
@code{port->list} curried with the appropriate parsers:

@codeblock{(port->string-list port) => (port->list read-line port)
           (port->sexp-list   port) => (port->list read port)}

The following compositions also hold:

@codeblock{run/string* => (compose port->string run/port*)
           run/strings* => (compose port->string-list run/port*)
           run/sexp* => (compose read run/port*)
           run/sexps* => (compose port->sexp-list run/port*)}
}


@defproc/nolink[(port-fold [port port?]
                                      [reader (-> port? any/c)]
                                      [op (-> any/c any/c ...+ (values any/c ...+))]
                                      [seeds any/c] ...+) (values any/c ...+)]{
This procedure can be used to perform a variety of iterative operations over an input stream. It
repeatedly uses @code{reader} to read an object from @code{port}. If the first read returns eof,
then the entire @code{port-fold} operation returns the seeds as multiple values.

If the first read operation returns some other value @code{v}, then @code{op} is applied to @code{v}
and the seeds: @code{(op v . seeds)}. This should return a new set of seed values, and the reduction
then loops, reading a new value from the port, and so forth. If multiple seed values are used, then
@code{op} must return multiple values.

For example,

@codeblock{(port->list reader port)}

could be defined as

@codeblock{(reverse (port-fold port reader cons '()))}

An imperative way to look at @code{port-fold} is to say that it abstracts the idea of a loop over a
stream of values read from some port, where the seed values express the loop state.
}