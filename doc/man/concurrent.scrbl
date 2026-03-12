#lang scribble/manual

@title[#:tag "threads-chapter"]{Threads and Optimistic Concurrency}

@local-table-of-contents[]

This chapter describes Scheme 48's thread system: Scheme 48 threads are fully preemptive; all threads 
(currently) run within a single operating system process. Scheme 48 allows writing customized, 
nested schedulers, and provides numerous facilities for the synchronization of shared-memory programs,
most importantly proposals for optimistic concurrency.

@hyperlink["https://www.s48.org/1.9.3/s48manual.pdf#chapter.7"]{Chapter 7. Threads. }

@section{Creating and Controlling threads}

The bindings described in this section are part of the threads structure.

Each thread is represented by a thread object.

@defproc*[([(spawn [thunk (-> any)]) thread]
           [(spawn [thunk (-> any)] [name symbol?]) thread])]{              
@code{spawn} creates a new thread, passes that thread to the current scheduler, and instructs the 
scheduler to run @var{thunk} in that thread. The @var{name} argument associates a symbolic name with 
the thread; it is purely for debugging purposes.
}

@deftogether[(@defproc[(sleep [time-in-milliseconds intger?]) unspecific]
              @defproc[(relinquish-timeslice) unspecific]
              @defproc[(terminate-current-thread) unspecific])]{              
@code{sleep} asks the scheduler to suspend the current thread for at least @var{time-in-milliseconds}
milliseconds before resuming it. 

Similarly, @code{relinquish-timeslice} instructs the scheduler to run another thread, thus 
relinquishing the timeslice of the current thread. Finally, @code{terminate-current-thread} terminates
the current thread.
}

@section[#:tag "thread-local-sec"]{Thread-local fluids}
<s48 fluids are local to a command level>
<thread-fluids>

@subsection[#:tag "deadlocks-sec"]{Deadlocks}
There is a bug in the current system that causes an erroneous deadlock error if threads are blocked 
waiting for signals and no other threads are available to run. 
A work around is to create a thread that sleeps for a long time, which
prevents any deadlock errors (including real ones):
@codeblock{
> ,open threads
> (spawn (lambda ()
    ; Sleep for a year
    (sleep (* 1000 60 60 24 365))))
}