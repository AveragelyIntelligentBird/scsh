#lang scribble/manual

@title{Environment variables}

@deftogether[(@defproc[(getenv [key string?]) string]
              @defproc[(setenv [key string?] [val (or string? #f)]) undefined])]{
  These functions get and set the process environment, stored in the external C variable 
  @code{char **environ}. An environment variable @var{key} is a string. 
  
  If an environment variable is set to a string @var{val}, then the process' global environment 
  structure is altered with an entry of the form "key=val". If @var{key} is #f, then any 
  entry for var is deleted. 
}

@defproc[(env->alist) (values string->string alist)]{
  The @code{env->alist} procedure converts the entire environment into an alist, e.g.,
  @codeblock{
    (("TERM" . "vt100")
     ("SHELL" . "/usr/local/bin/scsh") 
     ("PATH" . "/sbin:/usr/sbin:/bin:/usr/bin")
     ("EDITOR" . "emacs") 
     ...)
  }      
}

@defproc[(alist->env [alist (listof string->string)]) undefined]{
  @var{alist} must be an association list whose keys are all strings, and whose values are all 
  either strings or string lists. 
  
  String lists are converted to colon lists (see below). The @var{alist} is installed as the current
  Unix environment (i.e., converted to a null-terminated C vector of "var=val" strings which is
  assigned to the global @code{char **environ}).
  @codeblock{
    ;;; Note $PATH entry is converted 
    ;;; to /sbin:/usr/sbin:/bin:/usr/bin.
    (alist->env '(("TERM" . "vt100")
                  ("PATH" "/sbin" "/usr/sbin" "/bin")
                  ("SHELL" . "/usr/local/bin/scsh")))
  }

  Note that @code{env->alist} and @code{alist->env} are not exact inverses: @code{alist->env} will 
  convert a list value into a single colon-separated string, but @code{env->alist} will not parse 
  colon-separated values into lists. (See the $PATH element in the examples given for each procedure.)
}

  
@deftogether[(@defproc[(alist-delete [key string?] [alist (listof string->string)]) (values string->string alist)]
              @defproc[(alist-update [key string?] [val string?] [alist (listof string->string)]) (values string->string alist)]
              @defproc[(alist-compress [alist (listof string->string)]) (values string->string alist)])]{
  These three functions help the programmer manipulate alist tables in some generally useful ways. 
  They are all defined using @code{equal?} for key comparison.

  @code{alist-delete} deletes any entry labelled by value @var{key}. 

  @code{alist-update} deletes @var{key} from @var{alist}, then cons'es on a @code{(key . val)} entry.

  @code{alist-compress} compresses @var{alist} by removing shadowed entries. Example:
  @codeblock{
    ;;; Shadowed (1 . c) entry removed.
    (alist-compress '( (1 . a) (2 . b) (1 . c) (3 . d) ))
        ==>  ((1 . a) (2 . b) (3 . d))
  }
}
        
@deftogether[(@defproc[(with-env* [env-alist-delta (listof string->string)] [thunk (-> any)]) (values value/s of thunk)]
              @defform[(with-env  [env-alist-delta (listof string->string)] body ...+)])]{
  @code{with-env*} calls @var{thunk} in the context of an altered environment. It returns whatever 
  values @var{thunk} returns. Non-local returns restore the environment to its outer value; 
  throwing back into the thunk by invoking a stored continuation restores the environment back to its 
  inner value.

  The @var{env-alist-delta} argument specifies a modification to the current environment -- the thunk's 
  environment is the original environment overridden with the bindings specified by the alist delta.

  @code{with-env} is a special form that provides syntactic sugar for @code{with-env*}. The env 
  @var{env-alist-delta} is not evaluated positions, but are implicitly backquoted. 
  In this way, it tends to resemble binding lists for let and let* forms.   

  Example: These four pieces of code all run the mailer with special $TERM and $EDITOR values.
  @codeblock{
    (with-env (("TERM" . "xterm") ("EDITOR" . ,my-editor))
      (run (mail shivers,@"@"lcs.mit.edu)))
    [0]
    (with-env* `(("TERM" . "xterm") ("EDITOR" . ,my-editor))
      (lambda () (run (mail shivers,@"@"csd.hku.hk))))
    [0]
    (run (begin (setenv "TERM" "xterm")      ; Env mutation happens
                (setenv "EDITOR" my-editor)  ; in the subshell.
                (exec-epf (mail shivers,@"@"research.att.com))))
    [0]
    ;; In this example, we compute an alternate environment ENV2
    ;; as an alist, and install it with an explicit call to the
    ;; EXEC-PATH/ENV procedure.
    (let* ((env (env->alist))           ; Get the current environment,
           (env1 (alist-update env  "TERM" "xterm"))      ; and compute
           (env2 (alist-update env1 "EDITOR" my-editor))) ; the new env.
      (run (begin (exec-path/env "mail" env2 "shivers,@"@"cs.cmu.edu"))))
  } 
}

@deftogether[(@defproc[(with-total-env* [env-alist (listof string->string)] [thunk (-> any)]) (values value/s of thunk)]
              @defform[(with-total-env  [env-alist (listof string->string)] body ...+)])]{
  Exactly the same as above, except @var{env-alist} argument specifies a complete environment that is 
  installed for thunk. 
}

@section{Path lists and colon lists}

When environment variables such as @code{$PATH} need to encode a list of strings (such as a list of 
directories to be searched), the common Unix convention is to separate the list elements with colon 
delimiters(...and hope the individual list elements don't contain colons themselves). 

To convert between the colon-separated string encoding and the list-of-strings representation, 
see the @code{infix-splitter} function <TODO LINK 8.1.2> and the string library's string-join function. 
For example,
@codeblock{
  (define split (infix-splitter (rx ":")))
  (split "/sbin:/bin::/usr/bin")  
    ; ==> '("/sbin" "/bin" "" "/usr/bin")
  (string-join ":" '("/sbin" "/bin" "" "/usr/bin"))
    ; ==> "/sbin:/bin::/usr/bin"
}
    
The following two functions are useful for manipulating these ordered lists, once they have been 
parsed from their colon-separated form.

@deftogether[(@defproc[(add-before [elt any] [before any] [list (listof any)]) list]
              @defproc[(add-after  [elt any] [after any] [list (listof any)]) list])]{
  These functions are for modifying search-path lists, where element order is significant.
  
  @code{add-before} adds @var{elt} to the @var{list} immediately before the first occurrence of @var{before} 
  in the list. If @var{before} is not in the list, @var{elt} is added to the end of the list.

  @code{add-after} is similar: @var{elt} is added after the last occurrence of @var{after}.
  If @var{before} is not found, elt is added to the beginning of the @var{list}.

  Neither function destructively alters the original path-list. The result may share structure 
  with the original list. Both functions use @code{equal?} for comparing elements.
}

@section{$USER, $HOME, and $PATH}

Like @code{sh} and unlike @code{csh}, scsh has no interactive dependencies on environment variables. 
It does, however, initialise certain internal values at startup time from the initial process 
environment, in particular @code{$HOME} and @code{$PATH}. Scsh never uses @code{$USER} at all. 

@defproc[(user-login-name) string]{
  This routine gets the current user's login name with @code{getlogin()}. 
}

@deftogether[(@defthing[#:kind "global" home-directory string]
              @defthing[#:kind "thread fluid" exec-path-list (values string list thread-fluid)])]{
  Scsh accesses @code{$HOME} at start-up time, and stores the value in the global variable 
  @code{home-directory}. It uses this value for @code{~} lookups and for returning to home on 
  @code{(chdir)}.

  Scsh accesses @code{$PATH} at start-up time, colon-splits the path list, and stores the value in 
  the thread fluid @code{exec-path-list}. This list is used for @code{exec-path} and 
  @code{exec-path/env} searches.

  To access, rebind or side-effect thread-fluid cells, you must open the @code{thread-fluids} package.} 
@; TODO - thread fluid package?? Should we expose that to users?
    