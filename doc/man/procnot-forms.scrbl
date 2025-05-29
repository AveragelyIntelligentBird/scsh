#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title{Process Forms}
A @emph{process form} specifies a computation to perform as an independent Unix process. It can be
one of the following:

@codeblock{(begin . scheme-code)            ; Run scheme-code in a fork.
           (| pf-1 ... pf-n)                ; Simple pipeline
           (|+ connect-list pf-1 ... pf-n)  ; Complex pipeline
           (epf . epf)                      ; An extended process form.
           (prog arg-1 ... arg-n)           ; Default: exec the program.}

The default case @code{(prog arg-1 ... arg-n)} is also implicitly backquoted. That is, it is
equivalent to:

@codeblock{(begin (apply exec-path `(prog arg-1 ... arg-n)))}

@code{exec-path} is the version of the
@hyperlink["http://www.FreeBSD.org/cgi/man.cgi?query=exec&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html"]{@code{exec()}}
system call that uses scsh's path list to search for an executable. The program and the arguments
must be either strings, symbols, or integers. Symbols and integers are coerced to strings. A
symbol's print-name is used. Integers are converted to strings in base 10. Using symbols instead of
strings is convenient, since it suppresses the clutter of the surrounding @code{"..."} quotation
marks. To aid this purpose, scsh reads symbols in a case-sensitive manner, so that you can say

@codeblock{(more Readme)}

and get the right file.

A @code{connect-list} is a specification of how two processes are to be wired together by pipes. It
has the form @code{((from-1 from-2 ... to) ...)} and is implicitly backquoted. For example,

@codeblock{(\|+ ((1 2 0) (3 1)) pf-1 pf-2)}
@; fix the need for \

runs @code{pf-1} and @code{pf-2}. The first clause @code{(1 2 0)} causes @code{pf-1}'s stdout (1)
and stderr (2) to be connected via pipe to @code{pf-2}'s stdin (0). The second clause @code{(3 1)}
causes @code{pf-1}'s file descriptor 3 to be connected to @code{pf-2}'s file descriptor 1.

The @code{begin} process form does a @code{stdio->stdports} synchronisation in the child process
before executing the body of the form. This guarantees that the @code{begin} form, like all other
process forms, ``sees'' the effects of any associated I/O redirections.

Note that RnRS does not specify whether or not @exec{|} and @exec{|+} are readable symbols. Scsh
does.