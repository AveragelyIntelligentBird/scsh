#lang scribble/manual

@title[#:style 'toc]{System Calls}
Scsh provides (almost) complete access to the basic Unix kernel services: processes, files, signals
and so forth. This chapter describes Scheme bindings for POSIX features, with a few of the more 
standard extensions thrown in (e.g., symbolic links, @code{fchown}, @code{fstat}). Socket handling 
is discussed separately in @secref["networking-chapter"].

@local-table-of-contents[]

@include-section["syscalls/errnos.scrbl"]
@include-section["syscalls/io.scrbl"]
@include-section["syscalls/filesystem.scrbl"]
@include-section["syscall-proc.scrbl"]
@include-section["syscalls/proc-state.scrbl"]
@include-section["syscalls/db-access.scrbl"]
@include-section["syscalls/cli-args.scrbl"]
@include-section["syscalls/sys-param.scrbl"]
@include-section["syscall-signal.scrbl"]
@include-section["syscalls/time-and-date.scrbl"]
@include-section["syscalls/environment-vars.scrbl"]
@include-section["syscall-tty.scrbl"]

