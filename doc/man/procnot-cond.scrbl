#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title{Conditional Process Sequencing Forms}
These forms allow conditional execution of a sequence of processes.

@defform/nolink[(|| pf ...+)]{
Run each proc until one completes successfully (i.e., exit status zero). Return true if some proc
completes successfully; otherwise @code{#f}.
}

@defform/nolink[(&& pf ...+)]{
Run each proc until one fails (i.e., exit status non-zero). Return true if all procs complete
successfully; otherwise @code{#f}.
}