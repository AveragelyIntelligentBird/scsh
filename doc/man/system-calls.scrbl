#lang scribble/manual

@title[#:style 'toc]{System Calls}
Scsh provides (almost) complete access to the basic Unix kernel services: processes, files, signals
and so forth. This chapter describes Scheme bindings for POSIX features, with a few of the more 
standard extensions thrown in (e.g., symbolic links, @code{fchown}, @code{fstat}). Socket handling 
is discussed separately in @secref["networking-chapter"].

@local-table-of-contents[]

@include-section["syscall-err.scrbl"]
@include-section["syscall-io.scrbl"]
@include-section["syscalls/filesystem.scrbl"]
@include-section["syscall-proc.scrbl"]
@include-section["syscall-proc-state.scrbl"]
@include-section["syscall-db-acc.scrbl"]
@include-section["syscalls/cli-args.scrbl"]
@include-section["syscall-sys-param.scrbl"]
@include-section["syscall-signal.scrbl"]
@include-section["syscall-time.scrbl"]
@include-section["syscall-env.scrbl"]
@include-section["syscall-tty.scrbl"]

