#lang scribble/manual

@title{User and group database access}

These procedures are used to access the user and group databases (e.g., the ones traditionally 
stored in @code{/etc/passwd} and @code{/etc/group}.)

@defproc[(user-info [uid/name (or integer? string?)]) user-info]{
  Return a @code{user-info} record giving the recorded information for a particular user, specified by
  @var{uid/name}. The @var{uid/name} argument is either an integer uid or a string user-name. 

  The is exposed as given below:
  @codeblock{
      (define-record-type 
        ; --- ;
        user-info?
        (name     user-info:name)
        (uid      user-info:uid)
        (gid      user-info:gid)
        (home-dir user-info:home-dir)
        (shell    user-info:shell))
  }

  The record is discriminated with the @as-index[@code{user-info?}] predicate. @code{uid} and @code{gid}
  fields are numbers, all other fields are strings.  All of the field accessors for the 
  @code{user-info} record are exposed to the @code{scsh-user} top-level package.
}

@deftogether[(@defproc[(->uid [uid/name (or integer? string?)]) integer]
              @defproc[(->username [uid/name (or integer? string?)]) string])]{
  These two procedures coerce integer uid's and user names to a particular form. 
}

@defproc[(group-info [gid/name (or integer? string?)]) group-info]{
  Return a @code{group-info} record giving the recorded information for a particular group, specified by
  @var{gid/name}. The @var{gid/name} argument is either an integer gid or a string group-name. 

  The is exposed as given below:
  @codeblock{
      (define-record-type 
        ; --- ;
        group-info?
        (name    group-info:name)
        (gid     group-info:gid)
        (members group-info:members))
  }

  The record is discriminated with the @as-index[@code{group-info?}] predicate. 
  @code{gid} is a number field, @code{name} is a string field and @code{members} is a list of strings.
  All of the field accessors for the @code{group} record are exposed to the @code{scsh-user} 
  top-level package.
}

@deftogether[(@defproc[(->gid [gid/name (or integer? string?)]) integer]
              @defproc[(->groupname [gid/name (or integer? string?)]) string])]{
  These two procedures coerce integer gid's and group names to a particular form. 
}
