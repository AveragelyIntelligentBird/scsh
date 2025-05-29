#lang scribble/manual

@title{Process Notation}
Scsh has a notation for controlling Unix processes that takes the form of s-expressions; this
notation can then be embedded inside of standard Scheme code. The basic elements of this notation
are @emph{process forms}, @emph{extended process forms}, and @emph{redirections}.

@local-table-of-contents[]

@include-section["procnot-gen.scrbl"]
@include-section["procnot-forms.scrbl"]
@include-section["procnot-usage.scrbl"]
@include-section["procnot-complex.scrbl"]
@include-section["procnot-cond.scrbl"]
@include-section["procnot-filters.scrbl"]
