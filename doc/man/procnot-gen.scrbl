#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title[#:tag "extended-process-forms-and-io-redirection"]{Extended Process Forms and I/O Redirections}
An @emph{extended process form} is a specification of a Unix process to run, in a particular I/O
environment:

@codeblock{epf ::= (pf redir-1 ... redir-n)}

where @code{pf} is a process form and the @code{redir}s are redirection specs.

A @emph{redirection spec} is one of:

@tabular[#:sep @hspace[1]
         (list (list @code{(< [fdes] file-name)} "Open file for read.")
               (list @code{(> [fdes] file-name)} "Open file create/truncate.")
               (list @code{(<< [fdes] object)} @para{Use @code{object}'s printed rep.})
               (list @code{(>> [fdes] file-name)} "Open file for append.")
               (list @code{(= [fdes] fdes/port)} "Dup2")
               (list @code{(- fdes/port)} @nested{Close @code{fdes/port}})
               (list @code{stdports} "0,1,2 dup'd from standard ports."))]

The input redirections default to file descriptor 0; the output redirections default to file
descriptor 1.

The subforms of a redirection are implicitly backquoted, and symbols stand for their print-names.
So @code{(> ,x)} means ``output to the file named by Scheme variable @code{x},'' and
@code{(< /usr/shivers/.login)} means ``read from  @code{/usr/shivers/.login}.''

Here are two more examples of I/O redirection:

@codeblock{(< ,(vector-ref fv i)) 
           (>> 2 /tmp/buf)}

These two redirections cause the file @code{fv[i]} to be opened on stdin, and @code{/tmp/buf} to be
opened for append writes on stderr.

The redirection @code{(<< object)} causes input to come from the printed representation of
@code{object}. For example,
         
@codeblock{(<< "The quick brown fox jumped over the lazy dog.")}

causes reads from stdin to produce the characters of the above string. The object is converted to
its printed representation using the @code{display} procedure, so

@codeblock{(<< (A five element list))}

is the same as

@codeblock{(<< "(A five element list)")}

is the same as

@codeblock{(<< ,(reverse '(list element five A)))}

(Here we use the implicit backquoting feature to compute the list to be printed.)

The redirection @code{(= fdes fdes/port)} causes @code{fdes/port} to be dup'd into file descriptor
@code{fdes}. For example, the redirection

@codeblock{(= 2 1)}

causes stderr to be the same as stdout. @code{fdes/port} can also be a port, for example:

@codeblock{(= 2 ,(current-output-port))}

causes stderr to be dup'd from the current output port. In this case, it is an error if the port is
not a file port (e.g., a string port).

More complex redirections can be accomplished using the @code{begin} process form, discussed below,
which gives the programmer full control of I/O redirection from Scheme.


@section{Port and File Descriptor Sync}
It's important to remember that rebinding Scheme's current I/O ports (e.g., using
@code{call-with-input-file} to rebind the value of @code{(current-input-port)}) does @emph{not}
automatically ``rebind'' the file referenced by the Unix stdio file descriptors 0, 1, and 2.
This is impossible to do in general, since some Scheme ports are not representable as Unix file
descriptors. For example, many Scheme implementations provide ``string ports,'' that is, ports that
collect characters sent to them into memory buffers. The accumulated string can later be retrieved
from the port as a string. If a user were to bind @code{(current-output-port)} to such a port, it
would be impossible to associate file descriptor 1 with this port, as it cannot be represented in
Unix. So, if the user subsequently forked off some other program as a subprocess, that program
would of course not see the Scheme string port as its standard output.


To keep stdio synced with the values of Scheme's current I/O ports, use the special redirection
@code{stdports}. This causes 0, 1, 2 to be redirected from the current Scheme standard ports. It is
equivalent to the three redirections:

@codeblock{(= 0 ,(current-input-port))
           (= 1 ,(current-output-port))
           (= 2 ,(error-output-port))}

The redirections are done in the indicated order.  This will cause an error if one of the current
I/O ports isn't a Unix port (e.g., if one is a string port). This Scheme/Unix I/O synchronisation can
also be had in Scheme code (as opposed to a redirection spec) with the @code{(stdports->stdio)}
procedure.
