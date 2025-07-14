#lang scribble/manual

@title{Scsh Reference Manual}

@; @author["Olin Shivers III" "Anfisa Bogdanenko"]

@declare-exporting[scsh]

@section{About Type Annotations}

The procedures in this documentation use type annotations (like @racket[string] 
and @racket[number]) to indicate the expected kinds of values. These are for 
documentation purposes only - the shell does not enforce these types at runtime.

@table-of-contents[]

@include-section["introduction.scrbl"]
@include-section["process-notation.scrbl"]
@include-section["system-calls.scrbl"]
@include-section["networking.scrbl"]
@include-section["strings-and-chars.scrbl"]
@; @include-section["pattern-match-rx.scrbl"]
@include-section["delim-strings.scrbl"]
@; @include-section["awk.scrbl"]
@; @include-section["concurrent.scrbl"]
@include-section["miscellaneous.scrbl"]
@include-section["running-scsh.scrbl"]

@index-section[]
