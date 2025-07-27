#lang scribble/manual

@title[#:tag "manipulating-filenames"]{Strings and characters}

@local-table-of-contents[]

@section[#:tag "byte-vectors-sec"]{Bytes and Byte vectors}
Bytes are small integers between 0 and 255.
<talk about bytes of course sigh>

@section[#:tag "text-codecs-sec"]{Text encodings}
port-text-codec set-port-text-codec!
@subsection{Unicode: UTF-8, UTF-16, UTF-32}

srfi-75
srfi-14 is Unicode compliant

@subsection{ASCII}
@subsection{Latin-1}

@section{Manipulating file names}
@subsection[#:tag "manipulaiting-file-names-terminology"]{Terminology}
@subsection[#:tag "manipulaiting-file-names-procedures"]{Procedures}

@section{Other string manipulation facilities}
@section{Character predicates}
@section{Deprecated character-set procedures}