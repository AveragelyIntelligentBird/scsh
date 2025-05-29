#lang scribble/manual

@title[#:style 'toc]{System Calls}
Scsh provides (almost) complete access to the basic Unix kernel services: processes, files, signals
and so forth.  These procedures comprise a Scheme binding for Posix, with a few of the more standard
extensions thrown in (e.g., symbolic links, @code{fchown}, @code{fstat}, sockets).

@local-table-of-contents[]

@include-section["syscall-err.scrbl"]
@include-section["syscall-io.scrbl"]
@include-section["syscall-fs.scrbl"]
@include-section["syscall-proc.scrbl"]
@include-section["syscall-proc-state.scrbl"]
@include-section["syscall-db-acc.scrbl"]
@include-section["syscall-cli-args.scrbl"]
@include-section["syscall-sys-param.scrbl"]
@include-section["syscall-signal.scrbl"]
@include-section["syscall-time.scrbl"]
@include-section["syscall-env.scrbl"]
@include-section["syscall-tty.scrbl"]

