#lang scribble/manual 

@title[#:tag "signals-chapter"]{Signal System}

@margin-note{
  Earlier releases of scsh had its own version of synchronous interface for asynchronous signals
  called @emph{sigevent} (courtesy of Olin Shivers). This idea was expanded upon by Scheme48 team
  to allow multiple mutable signal queues. We favor Scheme48 interface for this system.
}
Scsh is implemented on top of the Scheme 48 virtual machine, which means that signal delivery is to 
a scsh process is indirect. Unix signals are delivered in two stages: first, Unix delivers the signal 
to the Scheme 48 virtual machine, then the Scheme 48 virtual machine delivers the signal to the 
executing Scheme program as a Scheme 48 @code{signal} object. This ensures that signal delivery happens 
between two VM instructions, keeping individual instructions atomic. This also means, that raw
interrupt handlers are abstracted away from the user. The following chapter describes a synchronous 
interface to the asynchronous signals delivered by the operation system.

Scheme48's signal system provides the basis, but scsh makes some systems programming-oriented
@seclink["sync-signals-sec"]{extensions} to the signal model. 

@section{Named and Anonymous Signals}

There are two varieties of signals available, @italic{named} and @italic{anonymous}. A named signal 
is one for which we have a symbolic name, such as @code{(signal kill)} or @code{(signal pipe)}. 

Anonymous signals, for which we only have the current operating system's signal number, 
have no meaning in other operating systems. Named signals preserve their meaning in image files. 
Not all named signals are available from all OS's and there may be multiple names for a single OS 
signal number.

@deftogether[(@defproc[(integer->signal  [number intger?]) signal]
              @defproc[(name->signal     [name symbol?]) (values signal or #f)])]{              
The signal returned by @code{integer->signal} is a named signal if integer corresponds to a named 
signal in the current operating system; otherwise it returns an anonymous signal. 
@code{name->signal} returns a (named) signal or #f if the the signal name is not supported by 
the operating system. 
}

@deftogether[(@defproc[(signal?     [x any]) boolean]
              @defproc[(signal=?    [signal0 signal?] [signal1 signal?]) boolean]
              @defproc[(signal-name [signal signal?]) (values symbol or #f)]
              @defproc[(signal-os-number [signal signal?]) integer])]{     
@code{signal?} is a predicate for signals. @code{signal=?} returns #t if @code{signal0} and 
@code{signal1} have the same operating system number and #f if they do not. @code{signal-name} returns
a symbol if signal is named and #f if it is anonymous. @code{signal-os-number} returns an integer 
associated with the given signal, named or anonymous. 
}

@subsection[#:tag "named-signals-sec"]{Named Signals List}

Scsh uses an enumeration @code{signal} for representing named singals. The syntax for @code{signal} 
and valid @var{signal-name}s are as follows:
@defform[(signal signal-name)]
@tabular[
  #:style 'boxed
  #:row-properties '((bottom-border top-border) () () () () () () () () () () () () () () () () () () () () () () () () () () () () bottom-border ())
  #:sep @hspace[1]
     (list 
      (list ""  @bold{Name} @bold{Description})
      (list "" "" @emph{POSIX signals, P1990 and P2001})
      (list ""  @code{abrt} "Abnormal termination (as by abort())")
      (list ""  @code{alrm} "Timeout signal (as by alarm())")
      (list ""  @code{bus}  "Bus error, i.e. access to undefined portion of memory")
      (list ""  @code{chld} "Child stopped or terminated")
      (list ""  @code{cont} "Continue executing if stopped")
      (list ""  @code{fpe}  "Erroneous arithmetic operation, e.g. floating point exception")
      (list ""  @code{hup}  "Hangup detected on controlling terminal or death of controlling process")
      (list ""  @code{ill}  "Illegal instruction")
      (list ""  @code{int}  "Interrupt, interaction attention")
      (list ""  @code{kill} "Kill signal, cannot be caught or ignored")
      (list ""  @code{pipe} "Broken pipe: write to pipe with no readers")
      (list ""  @code{poll} "Pollable event (Sys V). A synonym for SIGIO")
      (list ""  @code{prof} "Profiling timer expired")
      (list ""  @code{quit} "Quit from keyboard, interaction termination")
      (list ""  @code{segv} "Segmentation violation, invalid memory reference")
      (list ""  @code{stop} "Stop process, cannot be caught or ignored")
      (list ""  @code{tstp} "Stop from keyboard, interactive stop request")
      (list ""  @code{sys}  "Bad system call (SVr4)")
      (list ""  @code{term} "Termination signal")
      (list ""  @code{trap} "Trace or breakpoint trap")
      (list ""  @code{ttin} "Attempted read from TTY by background process")
      (list ""  @code{ttou} "Attempted write to TTY by background process")
      (list ""  @code{urg}  "Urgent condition on socket (4.2 BSD)")
      (list ""  @code{usr1} "User-defined signal 1")
      (list ""  @code{usr2} "User-defined signal 2")
      (list ""  @code{vtalrm} "Virtual alarm clock (4.2BSD)")
      (list ""  @code{xcpu} "Exceeded CPU time limit (4.2BSD)")
      (list ""  @code{xfsz} "Exceeded file size limit (4.2 BSD)")
      ;  Additional signals from (RedHat) Linux
      (list "" "" @emph{Non-POSIX signals, present in Linux})
      (list ""  @code{cld}  "A synonym for SIGCHLD")
      (list ""  @code{emt}  "Emulator trap")
      (list ""  @code{info} "A synonym for SIGPWR")
      (list ""  @code{io}   "I/O now possible (4.2 BSD)")
      (list ""  @code{iot}  "I/O trap. A synonym for ABRT")
      (list ""  @code{lost} "File lock lost")
      (list ""  @code{pwr}  "Power failure (Sys V)")
      (list ""  @code{stkflt} "Stack fault on coprocessor")
      (list ""  @code{unused} "Unused signal")
      (list ""  @code{winch}  "Window resize signal (4.3 BSD, Sun)")
      )]

@section{Sending Signals}

@deftogether[(@defproc[(signal-process       [proc  (or integer? proc?)] [sig signal?]) unspecific]
              @defproc[(signal-process-group [prgrp (or integer? proc?)] [sig signal?]) unspecific])]{              
These two procedures send signals to a specific process, and all the processes in a specific 
process group, respectively. 

The @var{proc} and @var{prgrp} arguments are either processes or integer process ids.  
}

@section{Receiving Signals}

Signals received by the scsh process can be obtained via one or more @code{signal-queues}. 
Each signal queue has a list of monitored signals and a queue of received signals that have yet
to be read from the @code{signal-queue}. When the Scheme process receives a signal, it is added to 
the received signal queues of all @code{signal-queue}s which are currently monitoring that particular
signal.

Any signal can be processed by a signal queue except for @code{(signal kill)} and @code{(signal stop)},
which cannot be handled or ignored.

@deftogether[(@defproc[(make-signal-queue [list-of-signals (list-of signal?)]) signal-queue]
              @defproc[(signal-queue?     [x any]) boolean]
              @defproc[(signal-queue-monitored-signals [signal-queue signal-queue?]) 
                       (values list of signals)])]{
@code{make-signal-queue} returns a new @code{signal-queue} that will monitor the signals in the 
@var{list-of-signals}. @code{signal-queue?} is a predicate for signal queues. 
@code{signal-queue-monitored-signals} returns a list of the signals currently monitored by 
@var{signal-queue}.
}

@deftogether[(@defproc[(dequeue-signal!       [signal-queue signal-queue?]) signal]
              @defproc[(maybe-dequeue-signal! [signal-queue signal-queue?]) (values signal or #f)])]{
@code{dequeue-signal!} and @code{maybe-dequeue-signal!} both return the next 
received-but-unread signal from signal-queue. If @var{signal-queue}'s queue of signals is empty,
@code{dequeue-signal!} blocks until an appropriate signal is received. @code{maybe-dequeue-signal!} 
does not block; it returns #f instead.

There is a bug in the current system that causes an erroneous deadlock error if threads are blocked 
waiting for signals and no other threads are available to run. See a workaroud 
@seclink["deadlocks-sec"]{here}. 
}

@deftogether[(@defproc[(add-signal-queue-signal!    [signal-queue signal-queue?] [signal signal?]) unspecific]
              @defproc[(remove-signal-queue-signal! [signal-queue signal-queue?] [signal signal?]) unspecific]
              )]{
These two procedures can be used to add or remove signals from a @var{signal-queue}'s list of 
monitored signals. When a signal is removed from a @var{signal-queue}'s list of monitored signals any 
occurances of the signal are removed from that @var{signal-queue}'s pending signals. In other words, 
@code{dequeue-signal!} and @code{maybe-dequeue-signal!} will only return signals that are currently on
the @var{signal-queue}'s list of signals.
}

@subsection{Ignoring Signals}

In some rare situations it might be necessary to ignore the occurrence of a signal. This is easily 
done by creating a queue and never waiting on it. 

This particularly appplies to synchronous signals as defined @seclink["sync-signals-sec"]{here}. At 
the moment, there is no good way to handle these situations, so you might need to supress a 
synchronous signal and rely on the return value to communicate the fautly state. Thankfully, this 
redundancy is very common, for example in case @code{pipe}. 

@section[#:tag "sync-signals-sec"]{Synchronous Signals}
@margin-note{
  Earlier releases of scsh raised exceptions by default upon receiving synchronous signals. In the 
  current scheme48 this is a much more tricky task because of multithreading. Receiving a synchronous 
  signal means it could have been triggered by any thread, and there is no way to locate origin of 
  the call. As a possible solution, thread switching could be suppressed for the duration of 
  the call and @code{os-signal-handler} could be temporarily modified, but this would require much 
  tighter integration with Scheme48's interrupt system. This feature will be brought in as soon as 
  possible.
}
@emph{NOTE:} This functionality is not yet implemented, but it is important and deserves discussion 
for feature parity with previous releases.

Among the named signals, scsh recognises a special subset of @emph{synchronous} signals. These signals
are intended to communicate some issue to the caller, normally as a result of a failed function call.
It is not a formal definition, but it has practical utility since we can process these signals as 
regular Scheme exceptions. Below is a list of signals scsh considers synchronous:
@tabular[
  #:style 'boxed
  #:row-properties '((bottom-border top-border) () () () () () () () () () () bottom-border ())
  #:sep @hspace[1]
     (list 
      (list ""  @bold{Name} @bold{Description})
      (list "" "" @emph{Synchronous POSIX signals, P1990 and P2001})
      (list ""  @code{abrt} "Abnormal termination (as by abort())")
      (list ""  @code{bus}  "Bus error, i.e. access to undefined portion of memory")
      (list ""  @code{fpe}  "Erroneous arithmetic operation, e.g. floating point exception")
      (list ""  @code{ill}  "Illegal instruction")
      (list ""  @code{pipe} "Broken pipe: write to pipe with no readers")
      (list ""  @code{segv} "Segmentation violation, invalid memory reference")
      (list ""  @code{sys}  "Bad system call (SVr4)")
      (list ""  @code{trap} "Trace or breakpoint trap")
      (list ""  @code{ttin} "Attempted read from TTY by background process")
      (list ""  @code{ttou} "Attempted write to TTY by background process")
      (list "" "" @emph{Synchronous Non-POSIX signals, present in Linux})
      (list ""  @code{emt}  "Emulator trap")
      (list ""  @code{iot}  "I/O trap. A synonym for ABRT")
      )]

The semantics of rasing a Scheme exceptions upon receiving a synchronous signal is compelling, but
it would conflict with the concurrency-focused signal system of Scheme48. To avoid violating threading
guarantees, we can suppress thread-switching for the duration of the call of interest and install
a special exception-raising handler for just that call. 

@defproc[#:link-target? #f
        (with-sync-signal-handler* [handler (-> signal any)]
                                    [thunk (-> any)])
                                    (values value/s of thunk)]{
  If we receive a synchronous terminating signal while @var{thunk} is executing, @var{handler} is 
  called on the offending signal.
}

This procedure can be sugared over with the following syntax:
             
@defform[#:link-target? #f
        (with-sync-signal-handler handler-spec body ...+)
         #:grammar
         [(handler-spec ((offending-signal) clause ...))
          (clause (code:line ((signal-name ...) clause-body ...))
                  (else body ...))]]{
  This form executes the body forms with a particular @code{signal} handler installed. Note that the 
  @var{offending-signal} variable bound at the top of @var{handler-spec} is lexically visible to the 
  error clauses.     
  
  When synchronous signal is received, the handler search machinery will bind variable 
  @var{offending-signal} to the corresponding signal obeject. Then, the clauses will be checked for a
  match based on @var{signal-name}.
  
  The first clause that matches is executed, and its value is the value of the entire 
  @code{with-sync-signal-handler} form. If no clause matches, the OS signal exception is propagated up.

  Error clauses have two forms. In the first type of clause, the forms are valid @var{signal-name}s, 
  as defined @seclink["named-signals-sec"]{here}. An @code{else} clause matches any 
  @code{offending-signal} value. 
}