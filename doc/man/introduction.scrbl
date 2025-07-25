#lang scribble/manual

@title{Introduction}
This is the reference manual for @exec{scsh}, a Unix shell that is embedded within Scheme. Scsh is a
Scheme designed for writing useful standalone Unix programs and shell scripts---it spans a wide
range of application, from "script" applications usually handled with @exec{sh}, to more standard
systems applications usually written in C.

Scsh is built as a library on top of @hyperlink["http://s48.org"]{scheme48}, and has two components:
a process notation for running programs and setting up pipelines and redirections, and a syscall
library for low-level access to the operating system.

This manual gives a complete description of scsh. A general discussion of the design principles
behind scsh can be found in a companion paper "A Scheme Shell" (reference).

@section{Current Release}

<talk about goals and limitations of the current release, e.g. being a self-contained s48 package > 

<focused on Linux and freeBSD in this release, likely will generilze better in the future, pls let us know if we missed something>

@section{Obtaining Scsh}
The current version of scsh is still in development. We're using git as a source code management
system, and the primary repository is hosted on github at
@hyperlink["https://github.com/scheme/scsh"]{scheme/scsh}.

This section will be updated to point at the final distribution once the next version is released.

@; need to talk about interaction between racket and scsh/s48 conventions.

@section{Copyright & License}
Scsh is open source. The complete source comes with the standard distribution. Scsh has an
ideologically hip, BSD-style license.

We note that the code is a rich source for other Scheme implementations to mine. Not only the
@emph{code}, but the @emph{APIs} are available for implementors working on Scheme environments for
systems programming. These APIs represent years of work, and should provide a big head-start on any
related effort. (Just don't call it ``scsh,'' unless it's @emph{exactly} compliant with the scsh
interfaces.)

Take all the code you like; we'll just write more.
