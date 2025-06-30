#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title{Signal system}

There are two varieties of signals available, @italic{named} and @italic{anonymous}. A named signal is one for which we have a symbolic name, such as @code{kill} or @code{pipe}. A full list of named signals supported by scheme48 is given in @bold{3.9.5}.

Anonymous signals, for which we only have the current operating system's signal number, have no meaning in other operating systems. Named signals preserve their meaning in image files. Not all named signals are available from all OS's and there may be multiple names for a single OS signal number.

@defform/nolink[(signal signal-name)]{
      The syntax signal returns a (named) signal associated with signal-name. 
}

@deftogether[(
            @defproc/nolink[(signal-num->signal [number intger?]) signal]
            @defproc/nolink[(signal-name->signal [symbol symbol?]) (or/c signal? #f)]
              )]{              
The signal returned by @code{signal-num->signal} is a named signal if integer corresponds to a named signal in the current operating system; otherwise it returns an anonymous signal. @code{signal-name->signal} returns a (named) signal or #f if the the signal name is not supported by the
operating system. 
}

@deftogether[(
              @defproc/nolink[(signal? [x any]) boolean]
              @defproc/nolink[(signal=? [signal0 signal?] [signal1 signal?]) boolean]
              @defproc/nolink[(signal-name [signal signal?]) (or/c symbol? #f)]
              @defproc/nolink[(signal-os-number [signal signal?]) integer]
              )]{     
@code{signal?} is a predicate for signals. @code{signal=?} returns #t if @code{signal0} and @code{signal1} have the same operating system number and #f if they do not. @code{signal-name} returns a symbol if signal is named and #f if it is anonymous. @code{signal-os-number} returns an integer associated with the given signal, named or anonymous. 
}

@section{Interrupts and Signals}

Scsh is implemented on top of the Scheme 48 virtual machine, which means that signal delivery is to a scsh process is indirect. Unix signals are delivered in two stages: first, Unix delivers the signal to the Scheme 48 virtual machine, then the Scheme 48 virtual machine delivers the signal to the executing Scheme program as a Scheme 48 @code{signal} or @code{interrupt}. This ensures that signal delivery happens between two VM instructions, keeping individual instructions atomic.

<insert discussion on signals versus interrupts here>

This difference is irrelevant for sending out signals since we cannot affect how other programs deal with them, but it becomes meaningful when scsh @italic{receives} a Unix signal. As a result, scsh provides a different interface for processing each group.

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Interrupt}   @bold{Unix signal} @bold{OS Variant})
               (list "interrupt/alrm"  	"signal/alrm" 	"POSIX")
               (list "interrupt/int"  	"signal/int" 	"POSIX")
               (list "interrupt/chld"  	"signal/chld" 	"POSIX")
               (list "interrupt/cont"  	"signal/cont" 	"POSIX")
               (list "interrupt/hup"  	"signal/hup" 	"POSIX")
               (list "interrupt/quit"  	"signal/quit" 	"POSIX")
               (list "interrupt/term"  	"signal/term" 	"POSIX")
               (list "interrupt/tstp"  	"signal/tstp" 	"POSIX")
               (list "interrupt/usr1"  	"signal/usr1" 	"POSIX")
               (list "interrupt/usr2"  	"signal/usr2" 	"POSIX")


               (list "interrupt/info"  	"signal/info" 	"BSD only")
               (list "interrupt/io"  	"signal/io" 	"BSD + SVR4")
               (list "interrupt/poll"  	"signal/poll" 	"SVR4 only")
               (list "interrupt/prof"  	"signal/prof" 	"BSD + SVR4")
               (list "interrupt/pwr"  	"signal/pwr" 	"SVR4 only")

               (list "interrupt/urg"  	"signal/urg" 	"BSD + SVR4")
               (list "interrupt/vtalrm"   "signal/vtalrm"   "BSD + SVR4")
               (list "interrupt/winch"    "signal/winch" 	"BSD + SVR4")
               (list "interrupt/xcpu"  	"signal/xcpu" 	"BSD + SVR4")
               (list "interrupt/xfsz"  	"signal/xfsz" 	"BSD + SVR4")
               
               )]

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Unix Signal}   @bold{Type} @bold{OS Variant})
               (list "signal/stop"  	"Uncatchable" 	"POSIX")
               (list "signal/kill"  	"Uncatchable" 	"POSIX")
               (list "signal/abrt"  	"Synchronous" 	"POSIX")
               (list "signal/fpe"  	    "Synchronous" 	"POSIX")
               (list "signal/ill"  	    "Synchronous" 	"POSIX")
               (list "signal/pipe"  	"Synchronous" 	"POSIX")
               (list "signal/segv"  	"Synchronous" 	"POSIX")
               (list "signal/ttin"  	"Synchronous" 	"POSIX")
               (list "signal/ttou"  	"Synchronous" 	"POSIX")
               (list "signal/bus"    	"Synchronous" 	"BSD + SVR4")
               (list "signal/emt"       "Synchronous"   "BSD + SVR4")
               (list "signal/iot"       "Synchronous" 	"BSD + SVR4")
               (list "signal/sys"   	"Synchronous" 	"BSD + SVR4")
               (list "signal/trap"  	"Synchronous" 	"BSD + SVR4")
               
               )]

@section{Sending Signals}

@deftogether[(@defproc/nolink[(signal-process [proc proc?] [sig signal?]) undefined]
              @defproc/nolink[(signal-process-group [prgrp proc?] [sig signal?]) undefined])]{              
These two procedures send signals to a specific process, and all the processes in a specific process group, respectively. The proc and prgrp arguments are either processes or integer process ids.  
}

@section{Receiving Synchronous Signals}

Synchronous occurrences of signals are better handled by raising a Scheme exception, which is the default behaviour for all incoming synchronous signals. There are, however, some rare situtations where it is necessary to ignore the occurrence of a synchronous signal. For this case, the following procedures exist:

@deftogether[(
              @defproc/nolink[(ignore-sync-signal [signal sync-signal?]) undefined]
              @defproc/nolink[(handle-sync-signal-default [signal sync-signal?]) undefined]
              )]{
    The procedure @code{ignore-sync-signal} tells the process to ignore the given signal. The procedure @code{handle-sync-signal-default} resets the signal handler to the default handler, i.e. raising a Scheme exception. Both these procedures will fail if @code{signal} is not a synchronous signal.
}

These procedures manipulate the core signal handler of the Scheme 48 VM, so they might undermine the behaviour of the signal system. They are intended to be used only for igoring synchronous signals if system calls cannot succeed otherwise. 

@section{Interrupts}

<custom interrupt, bring in a signal (named or anonymous) as an interrupt that we can monitor in queues?>

@deftogether[(
              @defproc/nolink[(signal->interrupt [signal signal?]) interrupt]
              @defproc/nolink[(signal-num->interrupt [signal-os-number integer?]) interrupt]
              @defproc/nolink[(interrupt? [x any]) boolean]
              )]{  

@code{signal->interrupt} converts a signal into an interrupt that can be monitored in interrupt queues. @code{signal-num->interrupt} creates an interrupt directly from an OS signal number.
}

@section{Receiving Interupts}

<insert dicussion on why interrupt handlers are not a thing in scsh>

Instead, scsh provides a synchronous interface to the asynchronous interrupts delivered by the operation system. 

<mention olin's OS resource management paper, say that s48 took that idea and expanded on it by allowing user to have multiple queues; we are also enforcing synchronous exceptions>

<sigevent???>

Interrupts received by the Scheme process can be obtained via one or more @code{interrupt-queue}. Each interrupt queue has a list of monitored interrupt and a queue of received interrupt that have yet to be read from the @code{interrupt-queue}. When the Scheme process receives an interrupt that interrupt is added to the received interrupt queues of all @code{interrupt-queue} which are currently monitoring that particular interrupt.

@deftogether[(
              @defproc/nolink[(make-interrupt-queue [list-of-interrupts list?]) interrupt-queue]
              @defproc/nolink[(interrupt-queue? [x any]) boolen]
              @defproc/nolink[(interrupt-queue-monitored-signals [interrupt-queue interrupt-queue?]) (list-of interrupt)]
              @defproc/nolink[(dequeue-interrupt! [interrupt-queue interrupt-queue?]) interrupt]
              @defproc/nolink[(maybe-dequeue-interrupt! [interrupt-queue interrupt-queue?]) (or/c interrupt #f)]
              )]{
@code{make-interrupt-queue} returns a new @code{interrupt-queue} that will monitor the signals in the list-of-interrupts @code{interrupt-queue?} is a predicate for interrupt queues. @code{interrupt-queue-monitored-signals}
returns a list of the signals currently monitored by interrupt-queue @code{dequeue-interrupt!}
and @code{maybe-dequeue-interrupt!} both return the next received-but-unread interrupt
from interrupt-queue If interrupt-queue's queue of signals is empty @code{dequeue-signal!}
blocks until an appropriate signal is received. @code{maybe-dequeue-signal!} does
not block; it returns #f instead.
}

<include discussion on deadlock bug>

@deftogether[(
              @defproc/nolink[(add-signal-queue-signal! [signal-queue signal-queue?] [signal signal?]) undefined]
              @defproc/nolink[(remove-signal-queue-signal! [signal-queue signal-queue?] [signal signal?]) undefined]
              )]{
These two procedures can be used to add or remove signals from a signal-
queue's list of monitored signals. When a signal is removed from a signal-
queue's list of monitored signals any occurances of the signal are removed from
that signal-queue's pending signals. In other words, dequeue-signal! and
maybe-dequeue-signal! will only return signals that are currently on the
signal-queue's list of signals.
}

<discussion on copatibility with old interface>

<we can provide with-enabled-interrupts>

@section{Miscellaneous}

@deftogether[(@defproc/nolink[(itimer [secs integer?]) undefined])]{              
Schedules a timer interrupt in @var{secs} seconds. Note: As the thread system needs the timer interrupt for its own purpose, itimer works by spawning a thread which calls the interrupt handler for interrupt/alrm after the specified time.
}

@section{All named signals in scheme48}
@tabular[#:style 'boxed
         (list (list @bold{Signal Name}   @bold{Explanation})
               (list "abrt" "abort - abnormal termination (as by abort())")
               (list "alrm" "alarm - timeout signal (as by alarm())")
               (list "fpe"  "floating point exception")
               (list "hup" "hangup - hangup on controlling terminal or death of controlling process")
               (list "ill" "illegal instruction")
               (list "int" "interrupt - interaction attention")
               (list "kill" "kill - termination signal, cannot be caught or ignored")
               (list "pipe" "pipe - write on a pipe with no readers")
               (list "quit" "quit - interaction termination")
               (list "segv" "segmentation violation - invalid memory reference")
               (list "term" "termination - termination signal")
               (list "usr1" "user1 - for use by applications")
               (list "usr2" "user2 - for use by applications")
               (list "chld" "child - child process stopped or terminated")
               (list "cont" "continue - continue if stopped")
               (list "stop" "stop - cannot be caught or ignored")
               (list "tstp" "interactive stop")
               (list "ttin" "read from control terminal attempted by background process")
               (list "ttou" "write to control terminal attempted by background process")
               (list "bus" "bus error - access to undefined portion of memory")
               ;  Additional signals from (RedHat) Linux
               (list "trap" "trace or breakpoint trap")
               (list "iot" "IOT trap - a synonym for ABRT")
               (list "emt" "ambulance?")
               (list "sys" "bad argument to routine (SVID)")
               (list "stkflt" "stack fault on coprocessor")
               (list "urg" "urgent condition on socket (4.2 BSD)")
               (list "io" "I/O now possible (4.2 BSD)")
               (list "poll" "A synonym for SIGIO (System V)")
               (list "cld" "A synonym for SIGCHLD")
               (list "xcpu" "CPU time limit exceeded	(4.2 BSD)")
               (list "xfsz" "File size limit exceeded	(4.2 BSD)")
               (list "vtalrm" "Virtual alarm clock (4.2 BSD)")
               (list "prof" "Profile alarm clock")
               (list "pwr" "Power failure (System V)")
               (list "info" "A synonym for SIGPWR")
               (list "lost" "File lock lost")
               (list "winch" "Window resize signal (4.3 BSD, Sun)")
               (list "unused" "Unused signal")
               )]