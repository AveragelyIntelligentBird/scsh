#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title{Filesystem}
Besides the procedures in this section, which allow access to the computer's file system, scsh also
provides a set of procedures which manipulate file @emph{names}. These string-processing procedures
are documented in @; TODO @secref["manipulating-filenames"]

@section{Manipulating Filesystem Objects}
@deftogether[(@defproc/nolink[(create-directory [fname string?] [mode file-mode? (file-mode all)] [override? (or/c #f 'query any/c) #f]) undefined]
              @defproc/nolink[(create-fifo [fname string?] [mode file-mode? (file-mode all)] [override? (or/c #f 'query any/c) #f]) undefined]
              @defproc/nolink[(create-hard-link [oldname string?] [newname string?] [override? (or/c #f 'query any/c) #f]) undefined]
              @defproc/nolink[(create-symlink [oldname string?] [newname string?] [override? (or/c #f 'query any/c) #f]) undefined])]{
These procedures create objects of various kinds in the file system. The @var{override?} argument
controls the action if there is already an object in the file system with the new name:

@tabular[#:sep @hspace[2]
         (list (list @code{#f} @nested{signal an error (default)})
               (list @code{'query} @nested{prompt the user})
               (list @emph{other} @nested{delete the old object (with @code{delete-file} or
                                          @code{delete-directory} as appropriate) before creating
                                          the new object}))]

@var{mode} defaults to @code{(file-mode all)} (but is masked by the current umask).
}

@deftogether[(@defproc/nolink[(delete-directory [fname string?]) undefined]
              @defproc/nolink[(delete-file [fname string?]) undefined]
              @defproc/nolink[(delete-filesys-object [fname string?]) undefined])]{
These procedures delete objects from the filesystem. The @code{delete-filesys-object} procedure will
delete an object of any type from the file system: files, (empty) directories, symlinks, fifos, etc.

If the object being deleted doesn't exist, @code{delete-directory} and @code{delete-file} raise an
error, while delete-filesys-object simply returns.
}

@defproc/nolink[(read-symlink [fname string?]) string?]{
Return the filename referenced by the symbolic link @var{fname}.
}

@margin-note{There is an unfortunate atomicity problem with the @code{rename-file} procedure: if you
             specify no-override, but create file @var{new-fname} sometime between
             @code{rename-file}'s existence check and the actual rename operation, your file will be
             clobbered with @var{old-fname}. There is no way to fix this problem, given the
             semantics of Unix @code{rename()}; at least it is highly unlikely to occur in
             practice.}

@defproc/nolink[(rename-file [old-fname string?] [new-fname string?] [override? (or/c #f 'query any/c) #f]) undefined]{
If you override an existing object, then @var{old-fname} and @var{new-fname} must
type-match---either both directories, or both non-directories. This is required by the semantics of
Unix @code{rename()}.
}

@deftogether[(@defproc/nolink[(set-file-mode [fname/fd/port (or/c string? integer? fdport?)] [mode file-mode?]) undefined]
              @defproc/nolink[(set-file-owner [fname/fd/port (or/c string? integer? fdport?)] [uid integer?]) undefined]
              @defproc/nolink[(set-file-group [fname/fd/port (or/c string? integer? fdport?)] [gid integer?]) undefined])]{
These procedures set the permission bits, owner id, and group id of a file, respectively. The file
can be specified by giving the file name, or either an integer file descriptor or a port open on the
file. Setting file user ownership usually requires root privileges.
}

@defproc/nolink[(set-file-times [fname string?]
                                           [access-time integer? (current-time)]
                                           [mod-time integer? (current-time)]) undefined]{
@var{fname} to the supplied values (see (link to sec:time) for the scsh representation of time). If
neither time argument is supplied, they are both taken to be the current time. You must provide both
times or neither. If the procedure completes successfully, the file's time of last status-change
(@code{ctime}) is set to the current time.
}

@margin-note{These procedures are not Posix. Their actual effect may vary between operating systems.
             See your OS's documentation for @code{sync(2)} and @code{fsync(2)} respectively for
             specifics}

@deftogether[(@defproc/nolink[(sync-file [fd/port (or/c integer? fdport?)]) undefined]
              @defproc/nolink[(sync-file-system) undefined])]{
Calling @code{sync-file} causes Unix to update the disk data structures for a given file. If
@var{fd/port} is a port, any buffered data it may have is first flushed. Calling
@code{sync-file-system} synchronises the kernel's entire file system with the disk.
}

@defproc/nolink[(truncate-file [fname/fd/port (or/c string? integer? fdport?)] [len integer?]) undefined]{
Truncate the specified file to @var{len} bytes in length.
}

@section{Querying File Information}
@defproc/nolink[(file-info [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) file-info?]{
Returns a record structure containing everything there is to know about a file. If the @var{chase?}
flag is true (the default), then the procedure chases symlinks and reports on the files to which
they refer. If @var{chase?} is false, then the procedure checks the actual file itself, even if it's
a symlink. The @var{chase?} flag is ignored if the file argument is a file descriptor or port.

The value returned is a @emph{file-info record}, whose accessors are defined in this chapter.
}

@defproc/nolink[(file-info:type [finfo file-info?])
         (or/c 'block-special 'char-special 'directory 'fifo 'regular 'socket 'symlink)]{
Returns a symbol denoting the type of the file described by @var{finfo}.
}

@defproc/nolink[(file-info:device [finfo file-info?]) integer?]{
Returns an integer denoting the device that the file described by @var{finfo} resides on.
}

@defproc/nolink[(file-info:inode [finfo file-info?]) integer?]{
Returns an integer denoting the file system inode that points to the file described by @var{finfo}.
}

@defproc/nolink[(file-info:mode [finfo file-info?]) file-mode?]{
Returns a file-mode object describing the permissions set on the file described by @var{finfo}.
}

@defproc/nolink[(file-info:nlinks [finfo file-info?]) integer?]{
Returns the number of hard links to the file described by @var{finfo}.
}

@defproc/nolink[(file-info:uid [finfo file-info?]) integer?]{
Returns user id of the owner of the file file described by @var{finfo}.
}

@defproc/nolink[(file-info:gid [finfo file-info?]) integer?]{
Returns the group id of the file described by @var{finfo}.
}

@defproc/nolink[(file-info:size [finfo file-info?]) integer?]{
Returns the size in bytes of the file described by @var{finfo}.
}

@defproc/nolink[(file-info:size [finfo file-info?]) integer?]{
Returns the time at which the file described by @var{finfo} was last accessed.
}

@defproc/nolink[(file-info:atime [finfo file-info?]) integer?]{
Returns the time at which the file described by @var{finfo} was last accessed.
}

@defproc/nolink[(file-info:mtime [finfo file-info?]) integer?]{
Returns the time at which the file described by @var{finfo} was last modified.
}

@defproc/nolink[(file-info:ctime [finfo file-info?]) integer?]{
Returns the time at which the file described by @var{finfo} last had its status changed.
}

@deftogether[(@defproc/nolink[(file:type [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t])
                       (or/c 'block-special 'char-special 'directory 'fifo 'regular 'socket 'symlink)]
              @defproc/nolink[(file:device [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) integer?]
              @defproc/nolink[(file:inode [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) integer?]
              @defproc/nolink[(file:mode [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) file-mode?]
              @defproc/nolink[(file:nlinks [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) integer?]
              @defproc/nolink[(file:uid [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) integer?]
              @defproc/nolink[(file:gid [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) integer?]
              @defproc/nolink[(file:size [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) integer?]
              @defproc/nolink[(file:atime [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) integer?]
              @defproc/nolink[(file:mtime [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) integer?]
              @defproc/nolink[(file:ctime [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) integer?])]{
These procuders are a composition of @code{file-info} and its accessors. They allow more convenient
access to file based information, without handling an intermediary file-info object.
}

@deftogether[(@defproc/nolink[(file-directory? [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) boolean?]
              @defproc/nolink[(file-fifo? [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) boolean?]
              @defproc/nolink[(file-regular? [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) boolean?]
              @defproc/nolink[(file-socket? [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) boolean?]
              @defproc/nolink[(file-special? [fname/fd/port (or/c string? integer? fdport?)] [chase? any/c #t]) boolean?]
              @defproc/nolink[(file-symlink? [fname/fd/port (or/c string? integer? fdport?)]) boolean?])]{
These procedures are file-type predicates that test the type of a given file. They are applied to
the same arguments to which @code{file-info} is applied; the sole exception is @code{file-symlink?},
which does not take the optional @var{chase?} second argument.
}

@deftogether[(@defproc/nolink[(file-info-directory? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-fifo? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-regular? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-socket? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-special? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-symlink? [file-info file-info?]) boolean?])]{
These are variants of the file-type predicates which work directly on @code{file-info} records.
}

@deftogether[(@defproc/nolink[(file-not-readable? [fname/fd/port (or/c string? integer? fdport?)])
                       (or/c #f 'search-denied 'permission 'no-directory 'nonexistent)]
              @defproc/nolink[(file-not-writable? [fname/fd/port (or/c string? integer? fdport?)])
                       (or/c #f 'search-denied 'permission 'no-directory 'nonexistent)]
              @defproc/nolink[(file-not-executable? [fname/fd/port (or/c string? integer? fdport?)])
                       (or/c #f 'search-denied 'permission 'no-directory 'nonexistent)])]{
This set of procedures are a convenient means to work on the permission bits of a file. The meaning
of their return values are as follows:

@tabular[#:sep @hspace[2]
         (list (list @code{#f} @nested{Access permitted})
               (list @code{'search-denied} @nested{Can't stat --- a protected directory is blocking access})
               (list @code{'permission} @nested{Permission denied.})
               (list @code{'no-directory} @nested{Some directory doesn't exist.})
               (list @code{'nonexistent} @nested{File doesn't exist.}))]

A file is considered writeable if either (1) it exists and is writeable or (2) it doesn't exist and
the directory is writeable. Since symlink permission bits are ignored by the filesystem, these calls
do not take a @var{chase?} flag.

Note that these procedures use the process' @emph{effective} user and group ids for permission
checking. Posix defines an @code{access()} function that uses the process' real uid and gids. This
is handy for setuid programs that would like to find out if the actual user has specific rights;
scsh ought to provide this functionality (but doesn't at the current time).

There are several problems with these procedures. First, there's an atomicity issue. In between
checking permissions for a file and then trying an operation on the file, another process could
change the permissions, so a return value from these functions guarantees nothing. Second, the code
special-cases permission checking when the uid is root---if the file exists, root is assumed to have
the requested permission. However, not even root can write a file that is on a read-only file
system, such as a CD ROM. In this case, @code{file-not-writable?} will lie, saying that root has
write access, when in fact the opening the file for write access will fail. Finally, write
permission confounds write access and create access. These should be disentangled.

Some of these problems could be avoided if Posix had a real-uid variant of the @code{access()} call
we could use, but the atomicity issue is still a problem. In the final analysis, the only way to
find out if you have the right to perform an operation on a file is to try and open it for the
desired operation. These permission-checking functions are mostly intended for script-writing, where
loose guarantees are tolerated.
}

@deftogether[(@defproc/nolink[(file-readable? [fname/fd/port (or/c string? integer? fdport?)]) boolean?]
              @defproc/nolink[(file-writable? [fname/fd/port (or/c string? integer? fdport?)]) boolean?]
              @defproc/nolink[(file-executable? [fname/fd/port (or/c string? integer? fdport?)]) boolean?])]{
These procedures are the logical negation of the preceding @code{file-not-}* procedures. Refer to those
for a discussion of their problems and limitations. These procedures will only ever return @code{#t}
or @code{#f}, and not the symbols giving specific reasons.
}

@deftogether[(@defproc/nolink[(file-info-not-readable? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-not-writable? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-not-executable? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-readable? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-writable? [file-info file-info?]) boolean?]
              @defproc/nolink[(file-info-executable? [file-info file-info?]) boolean?])]{
There are variants of the file permission predicates which work directly on @code{file-info}
records.
}

@deftogether[(@defproc/nolink[(file-not-exists? [fname/fd/port (or/c string? integer? fdport?)]) (or/c boolean? 'search-denied)]
              @defproc/nolink[(file-exists? [fname/fd/port (or/c string? integer? fdport?)]) boolean?])]{
The meaning of the return values of @code{file-not-exists?} are as follows:

@tabular[#:sep @hspace[2]
         (list (list @code{#f} @nested{Exists})
               (list @code{#t} @nested{Doesn't exist.})
               (list @code{'search-denied} @nested{Some protected directory is blocking the search.}))]

@code{file-exists?} is simply the logical negation of @code{file-not-exists?}.
}

@section{Traversing Directories}
@defproc/nolink[(directory-files [dir string? (cwd)] [dotfiles? boolean? #f]) (listof string?)]{
Return the list of files in directory @var{dir}, which defaults to the current working directory.
The @var{dotfiles?} flag causes dot files to be included in the list. Regardless of the value of
@var{dotfiles?}, the two files @code{"."} and @code{".."} are @emph{never} returned.

The directory @var{dir} is not prepended to each file name in the result list. That is,

@codeblock{(directory-files "/etc")}

returns

@codeblock{("chown" "exports" "fstab" ...)}

@emph{not}

@codeblock{("/etc/chown" "/etc/exports" "/etc/fstab" ...)}

To use the files in returned list, the programmer can either manually prepend the directory:

@codeblock{(map (lambda (f) (string-append dir "/" f)) files)}

or cd to the directory before using the file names:

@codeblock{(with-cwd dir
             (for-each delete-file (directory-files)))}

or use the @code{glob} procedure, defined in this chapter.

A directory list can be generated by @code{(run/strings (ls))}, but this is unreliable, as
filenames with whitespace in their names will be split into separate entries.
@code{directory-files} is reliable.
}

@deftogether[(@defproc/nolink[(open-directory-stream [dir string?]) directory-stream?]
              @defproc/nolink[(directory-stream? [maybe-directory-stream any/c]) boolean?]
              @defproc/nolink[(read-directory-stream [directory-stream directory-stream?]) (or/c string? #f)]
              @defproc/nolink[(close-directory-stream [directory-stream directory-stream?]) undefined])]{
These functions implement a direct interface to the @code{opendir()} / @code{readdir()} /
@code{closedir()} family of functions for processing directory streams.
@code{(open-directory-stream dir)} creates a stream of files in the directory @var{dir}.
@code{directory-stream?} is a predicate that recognizes directory-streams.
@code{(read-directory-stream directory-stream)} returns the next file in the stream or @code{#f} if
no such file exists. Finally, @code{(close-directory-stream directory-stream)} closes the stream.
}

@section{Globbing}
@defproc/nolink[(glob [pattern string?] ...) (listof string)]{
@margin-note{Why bother to mention such a silly possibility? Because that is what sh does.}
Glob each pattern against the filesystem and return the sorted list. Duplicates are not removed.
Patterns matching nothing are not included literally.

C shell {a,b,c} patterns are expanded. Backslash quotes characters, turning off the
special meaning of {, }, @code{*}, [, ], and ?.

Note that the rules of backslash for Scheme strings and glob patterns work together to require four
backslashes in a row to specify a single literal backslash. Fortunately, it is very rare that a
backslash occurs in a Unix file name.

A glob subpattern will not match against dot files unless the first character of the subpattern is a
literal ".". Further, a dot subpattern will not match the files @code{"."} or @code{".."}
unless it is a constant pattern, as in @code{(glob "../*/*.c")}. So a directory's dot files can be
reliably generated with the simple glob pattern @code{".*"}.

Some examples.

All the C and #include files in my directory:
@codeblock{(glob "*.c" "*.h")}

All the C files in this directory and its immediate subdirectories:
@codeblock{(glob "*.c" "*/*.c")}

All the C files in the lexer and parser dirs:
@codeblock{(glob "lexer/*.c" "parser/*.c")
           (glob "{lexer,parser}/*.c")}

All the C files in the strange directory @code{"{lexer,parser}"}:
@codeblock{(glob "\\{lexer,parser\\}/*.c")}

All the files ending in @code{"*"}, e.g. @code{("foo*" "bar*")}:
@codeblock{(glob "*\\*")}

All files containing the string @code{"lexer"}, e.g. @code{("mylexer.c" "lexer1.notes")}:
@codeblock{(glob "*lexer*")}

Either @code{("lexer")} or @code{()}:
@codeblock{(glob "lexer")}

If the first character of the pattern (after expanding braces) is a slash, the search begins at
root; otherwise, the search begins in the current working directory.

If the last character of the pattern (after expanding braces) is a slash, then the result matches
must be directories, e.g.

@codeblock{(glob "/usr/man/man?/") => ("/usr/man/man1/" "/usr/man/man2/" ...)}

Globbing can sometimes be useful when we need a list of a directory's files where each element in
the list includes the pathname for the file.

Compare:
@codeblock{(directory-files "../include") => ("cig.h" "decls.h" ...)}

@codeblock{(glob "../include/*") => ("../include/cig.h" "../include/decls.h" ...)}
}

@defproc/nolink[(glob-quote [pattern string?]) string?]{
Returns a constant glob pattern that exactly matches @var{pattern}. All wild-card characters in
@var{pattern} are quoted with a backslash.

@codeblock{(glob-quote "Any *.c files?") => "Any \\*.c files\\?"}
}

@section{Temporary Files}
@defproc/nolink[(create-temp-file [prefix string? (fluid *temp-file-template*)]) string?]{
@code{create-temp-file} creates a new temporary file and return its name. The optional argument
specifies the filename prefix to use, and defaults to the value of @code{"$TMPDIR/<pid>"} if
@code{$TMPDIR} is set and to @code{"/var/tmp/<pid>"} otherwise, where @var{pid} is the current
process' id. The procedure generates a sequence of filenames that have @var{prefix} as a common
prefix, looking for a filename that doesn't already exist in the file system. When it finds one, it
creates it, with permission @code{(file-mode owner-read owner-write)} and returns the filename.
(The file permission can be changed to a more permissive permission with @code{set-file-mode} after
being created).

This file is guaranteed to be brand new. No other process will have it open. This procedure does not
simply return a filename that is very likely to be unused. It returns a filename that definitely did
not exist at the moment @code{create-temp-file} created it.

It is not necessary for the process' pid to be a part of the filename for the uniqueness guarantees
to hold. The pid component of the default prefix simply serves to scatter the name searches into
sparse regions, so that collisions are less likely to occur. This speeds things up, but does not
affect correctness.

Security note: doing I/O to files created this way in @code{"/var/tmp/"} is not necessarily secure.
General users have write access to @code{"/var/tmp/"}, so even if an attacker cannot access the new
temp file, he can delete it and replace it with one of his own. A subsequent open of this filename
will then give you his file, to which he has access rights. There are several ways to defeat this
attack,

@itemlist[@item{Use @code{temp-file-iterate}, to return the file descriptor allocated when the file
                is opened. This will work if the file only needs to be opened once.}
          @item{If the file needs to be opened twice or more, create it in a protected directory,
                e.g., @code{"$HOME"}}
          @item{Ensure that @code{"/var/tmp"} has its sticky bit set. This requires system
                administrator privileges}]

The actual default prefix used is controlled by the dynamic variable @code{*temp-file-template*},
and can be overridden for increased security. See @code{temp-file-iterate} for details..
}

@deftogether[(@defproc/nolink[(temp-file-iterate [maker (-> string? (values any/c ...))]
                                                 [template string? (fluid *temp-file-template*)]) (values any/c ...)]
              @defthing[#:kind "fluid" #:link-target? #f *temp-file-template* string?])]{
@code{temp-file-iterate} can be used to perform certain atomic transactions on the file system
involving filenames. Some examples:

@itemlist[@item{Linking a file to a fresh backup temp name.}
          @item{Creating and opening an unused, secure temp file.}
          @item{Creating an unused temporary directory.}]

This procedure uses @var{template} to generate a series of trial file names. @var{template} should
be a @code{format} control string and its default is taken from the value of the dynamic variable
@code{*temp-file-template*} which itself defaults to @code{"$TMPDIR/<pid>.~a"} if @code{$TMPDIR} is
set and @code{"/usr/tmp/<pid>.~a"} otherwise, where @var{pid} is the scsh process' pid. File names
are generated by calling @code{format} to instantiate the template's @code{"~a"} field with a
varying string. For increased security, a user may wish to change the template to use a directory
not allowing world write access (e.g., his home directory). Scsh uses scheme48's
@hyperlink["http://s48.org/1.9.2/manual/manual-Z-H-6.html#node_sec_5.14"]{@code{fluids}}
package to implement dynamic binding; see the documentation for that for details.

@var{maker} is a procedure which is serially called on each file name generated.  It must return at
least one value; it may return multiple values. If the first return value is @code{#f} or if
@var{maker} raises the @code{errno/exist} errno exception, @code{temp-file-iterate} will loop,
generating a new file name and calling @var{maker} again. If the first return value is true, the
loop is terminated, returning whatever value(s) @var{maker} returned.

After a number of unsuccessful trials, @code{temp-file-iterate} may give up and signal an error.

Thus, if we ignore its optional @var{prefix} argument, @code{create-temp-file} could be defined as:

@codeblock{(define (create-temp-file)
             (let ((options (file-options create exclusive))
                   (mode (file-mode owner-read owner-write)))
               (temp-file-iterate
                 (lambda (f)
                   (close (open-output-file f options mode)) f))))}

To rename a file to a temporary name:

@codeblock{(temp-file-iterate
             (lambda (backup)
               (create-hard-link old-file backup) backup)
                   ".#temp.~a") ; Keep link in cwd.
           (delete-file old-file)}

Recall that scsh reports syscall failure by raising an error exception, not by returning an error
code. This is critical to to this example---the programmer can assume that if the
@code{temp-file-iterate} call returns, it returns successully. So the following @code{delete-file}
call can be reliably invoked, safe in the knowledge that the backup link has definitely been
established.

To create a unique temporary directory:

@codeblock{(temp-file-iterate
             (lambda (dir) (create-directory dir) dir)
             "/var/tmp/tempdir.~a")}

Similar operations can be used to generate unique symlinks and fifos, or to return values other than
the new filename (e.g., an open file descriptor or port).
}

@defproc/nolink[(temp-file-channel) (values [input-port port?] [output-port port?])]{
This procedure can be used to provide an interprocess communications channel with arbitrary-sized
buffering. It returns two values, an input port and an output port, both open on a new temp file.
The temp file itself is deleted from the Unix file tree before @code{temp-file-channel} returns, so
the file is essentially unnamed, and its disk storage is reclaimed as soon as the two ports are
closed.

@code{temp-file-channel} is analogous to @code{port-pipe} with two exceptions:

@itemlist[@item{If the writer process gets ahead of the reader process, it will not hang waiting for
                some small pipe buffer to drain. It will simply buffer the data on disk. This is
                good.}
          @item{If the reader process gets ahead of the writer process, it will also not hang
                waiting for data from the writer process. It will simply see and report an end of
                file. This is bad.}]

In order to ensure that an end-of-file returned to the reader is legitimate, the reader and writer
must serialise their I/O. The simplest way to do this is for the reader to delay doing input until
the writer has completely finished doing output, or exited.
}
