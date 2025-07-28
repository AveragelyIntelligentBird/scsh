#lang scribble/manual

@title{System parameters}

@defproc[(system-name) string?]{
Returns the name of the host on which we are executing. This may be a local name, such as 
"solar", as opposed to a fully-qualified domain name such as "solar.csie.ntu.edu.tw."
}

@defproc[(uname) uname-record?]{

The value returned is a @code{uname} record, which is defined as given below:
@codeblock{
    (define-record-type 
        ; ... ;
        uname?
        (os-name    uname:os-name)    ; OS name.
        (node-name  uname:node-name)  ; Network node hostname.
        (release    uname:release)    ; OS release name.
        (version    uname:version)    ; OS version name.
        (machine    uname:machine))   ; Machine hardware name.
}

The record is discriminated with the @as-index[@code{uname?}] predicate. Each of the fields 
contains a string. Be aware that the node name does not necessarily correspond to the 
fully-qualified domain name.

All of the field accessors for the @code{uname} record are exposed to the @code{scsh-user} 
top-level package. However, it might be more convenient to use the following bindings built on 
top of @code{(uname)}:
}

@deftogether[(@defproc[(uname-os)       string?]
              @defproc[(uname-node)     string?]
              @defproc[(uname-release)  string?]
              @defproc[(uname-version)  string?]
              @defproc[(uname-machine)  string?])]{
These procuders bypass constuction of redundant @code{uname} record and return vaules for each
field directrly.}