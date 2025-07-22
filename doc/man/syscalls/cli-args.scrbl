#lang scribble/doc
@(require scribble/manual)

@title{Accessing command-line arguments}

@deftogether[(@defproc[(command-line) (listof string?)]
              @defthing[#:kind "global" command-line-arguments (listof string?)])]{

Calling @code{(command-line)} returns the complete argv string list, including the program. The list 
of strings @code{command-line-arguments} contains the arguments passed to the scsh process on the 
command line. So if we run a scsh program

@codeblock{ /usr/shivers/bin/myls -CF src }

then @code{(command-line)} returns

@codeblock{ ("/usr/shivers/bin/myls" "-CF" "src") }

and @code{command-line-arguments} is

@codeblock{ ("-CF" "src") }
 
@code{(command-line)} returns a fresh list each time it is called. In this way, the programmer can get
a fresh copy of the original argument list if @code{command-line-arguments} has been modified or is 
lexically shadowed. }

@defproc*[([(arg [arglist (listof string?)] [n integer?]) string?]
           [(arg  [arglist (listof string?)] [n integer?] [default string?]) string?])]{
These procedures are useful for accessing arguments from argument lists. @code{arg} returns the nth 
element of arglist. The index is 1-based. If @var{n} is too large, @var{default} is returned; if no 
@var{default}, then an error is signaled.
}

@defproc*[([(arg* [arglist (listof string?)] [n integer?]) string?]
           [(arg* [arglist (listof string?)] [n integer?] [default-thunk (-> any? string?)]) string?])]{
@code{arg*} is similar, except that the @var{default-thunk} is called to generate the default value.
}

@defproc*[([(argv [n integer?]) string?]
           [(argv [n integer?] [default string?]) string?])]{
@code{(argv n)} is simply @code{(arg (command-line) (+ n 1))}. The +1 offset ensures that the two 
forms
@codeblock{
    (arg command-line-arguments n)
    (argv n)
}
return the same argument (assuming the user has not rebound or modified @code{command-line-arguments}).

Example:
@codeblock{
    (if (null? command-line-arguments)
            (& (xterm -n ,host -title ,host
                      -name ,(string-append "xterm_" host)))
            (let* ((progname (file-name-nondirectory (argv 1)))
                   (title (string-append host ":" progname)))
              (& (xterm -n     ,title
                        -title ,title
                        -e     ,@"@"command-line-arguments))))
}

A subtlety: when the scsh interpreter is used to execute a scsh program, the program name reported 
in the head of the @code{(command-line)} list is the scsh program, @italic{not} the interpreter. 
For example, if we have a shell script in file @code{fullecho}:
@; TODO HOWWWWWWWWW DOES IT RUN FROM SCRIPTS?????????
@codeblock{
    #!/usr/local/bin/scsh -s
    !#
    (for-each (lambda (arg) (display arg) (display " "))
                (command-line))
}
and we run the program

@codeblock{fullecho hello world}

the program will print out

@codeblock{fullecho hello world}

not

@codeblock{/usr/local/bin/scsh -s fullecho hello world}

This argument line processing ensures that if a scsh program is subsequently compiled into a 
standalone executable or byte-compiled to a heap-image executable by the Scheme 48 virtual machine, 
its semantics will be unchanged -- the arglist processing is invariant. In effect, the 
@code{/usr/local/bin/scsh -s} is not part of the program; it's a specification for the machine to 
execute the program on, so it is not properly part of the program's argument list.
}

