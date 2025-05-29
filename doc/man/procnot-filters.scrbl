#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title{Process Filters}
These procedures are useful for forking off processes to filter text streams.

@defproc/nolink[(make-char-port-filter [filter (-> character? character?)]) procedure?]{
Returns a procedure that when called, repeatedly reads a character from the current input port,
applies @code{filter} to the character, and writes the result to the current output port. The
procedure returns upon reaching eof on the input port.

For example, to downcase a stream of text in a spell-checking pipeline, instead of using the Unix
@code{tr A-Z a-z} command, we can say:

@codeblock{(run (\| (delatex)
                   (begin ((char-filter char-downcase))) ; tr A-Z a-z
                   (spell)
                   (sort)
                   (uniq))
                (< scsh.tex)
                (> spell-errors.txt))}
}

@defproc/nolink[(make-string-port-filter [filter (-> string? string?)]
                                                    [buflen integer? 1024]) procedure?]{
Returns a procedure that when called, repeatedly reads a string from the current input port, applies
@code{filter} to the string, and writes the result to the current output port. The procedure returns
upon reaching eof on the input port.

The optional @code{buflen} argument controls the number of characters each internal read operation
requests; this means that @code{filter} will never be applied to a string longer than @code{buflen}
chars.
}