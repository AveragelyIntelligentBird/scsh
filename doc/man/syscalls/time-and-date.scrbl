#lang scribble/manual 

@title{Time}

Scsh's time system is fairly sophisticated, particularly with respect to its careful treatment of 
time zones. However, casual users shouldn't be intimidated; all of the complexity is optional, and 
defaulting all the optional arguments reduces the system to a simple interface.

@section{Terminology}

"UTC" and "UCT" stand for "Universal Coordinated Time", which is the official name for what is 
colloquially referred to as "Greenwich Mean Time".

POSIX allows a single time zone to specify two different offsets from UTC: one standard one, and o
ne for "summer time". Summer time is frequently some sort of daylight savings time.

The scsh time package consistently uses this terminology: we never say "GMT" or "DST"; 
we always say "UTC" and "Summer Time".

@section{Basic data types}

We have two types: time and date.

@margin-note{
  Physics pedants please note: The scsh authors live in a Newtonian universe. We disclaim 
  responsibility for calculations performed in non-ANSI standard light-cones.
}
A time specifies an instant in the history of the universe. It is location and timezone 
independent. A time is a real value giving the number of elapsed seconds since the Unix "epoch" 
(Midnight, January 1, 1970 UTC). Time values provide arbitrary time resolution, limited only by the
number system of the underlying Scheme system.

A date is a name for an instant in time that is specified relative to some location/timezone in the 
world, e.g.:
@verbatim{Friday October 31, 1994 3:47:21 pm EST. }

A date is represented by a @code{date} record, which provides one-second resolution. The structure of
the record follows POSIX @code{tm} struct, which is defined as given below:
@codeblock{
  (define-record-type 
    ; ... ;
    date?
    (seconds   date:seconds   set-date:seconds)   ; Seconds after the minute [0-59]
    (minute    date:minute    set-date:minute)    ; Minutes after the hour [0-59]
    (hour      date:hour      set-date:hour)      ; Hours since midnight [0-23]
    (month-day date:month-day set-date:month-day) ; Day of the month [1-31]
    (month     date:month     set-date:month)     ; Months since January [0-11]
    (year      date:year      set-date:year)      ; Years since 1900
    (tz-name   date:tz-name   set-date:tz-name)   ; Time zone name: #f or a string.
    (tz-secs   date:tz-secs   set-date:tz-secs)   ; Time zone offset: #f or an integer.
    (summer?   date:summer?   set-date:summer?)   ; Summer (Daylight Savings) time in effect?
    (week-day  date:week-day  set-date:week-day)  ; Days since Sunday [0-6]
    (year-day  date:year-day  set-date:year-day)) ; Days since Jan. 1 [0-365]
}

The record is discriminated with the @as-index[@code{date?}] predicate. All fields are integers with 
the exception of @code{tz-name} (string or #f), @code{tz-secs} (integer or #f) and @code{summer?} (boolean).
All of the field accessors and setters for the @code{date} record are exposed to the @code{scsh-user}
top-level package.

If the @code{tz-secs} field is given, it specifies the time zone's offset from UTC in seconds. If it is 
specified, the @code{tz-name} and @code{summer?} fields are ignored when using the date structure to 
determine a specific instant in time.

If the @code{tz-name} field is given, it is a time zone string such as "EST" or "HKT" understood by
the OS. Since POSIX time zone strings can specify dual standard/summer time zones (e.g., "EST5EDT"
specifies U.S. Eastern Standard/Eastern Daylight Time), the value of the @code{summer?} field is used 
to resolve the amiguous boundary cases. For example, on the morning of the Fall daylight savings 
change-over, 1:00am-2:00am happens twice. Hence the date 1:30 am on this morning can specify two 
different seconds; the @code{summer?} flag says which one.

A date with @code{tz-name = tz-secs = #f} is a date that is specified in terms of the system's current
time zone.

There is redundancy in the date data structure. For example, the @code{year-day} field is redundant with 
the @code{month-day} and @code{month} fields. Either of these implies the values of the @code{week-day}
field. @code{The summer?} and @code{tz-name} fields are redundant with the @code{tz-secs} field in 
terms of specifying an instant in time. This redundancy is provided because consumers of dates may 
want it broken out in different ways. 

The scsh procedures that produce date records fill them out completely. However, when date records
produced by the programmer are passed to scsh procedures, the redundancy is resolved by ignoring 
some of the secondary fields. This is described for each procedure below.

@defproc[(make-date [second integer?] [minute integer?] [hour integer?]
                    [month-day integer?] [month integer?] [year integer?]
                    [tz-name (or string? #f) #f] [tz-secs (or integer? #f) #f]
                    [summer? boolean? #f] [week-day integer? 0] [year-day integer? 0])
                    date]{
  When making a date record, the last five elements of the record are optional, and default to 
  #f, #f, #f, 0, and 0 respectively. This is useful when creating a date record to pass as an 
  argument to time. Other procedures, however, may refuse to work with these incomplete date records. 
}

@section[#:tag "time-zones-sec"]{Time zones}

Several time procedures take time zones as arguments. When optional, the time zone defaults to local 
time zone. Otherwise the time zone can be one of:
@tabular[#:style 'boxed
          #:row-properties '((bottom-border top-border) '())
          #:sep @hspace[3]
  (list (list "" @bold{Time Zone Option } @bold{Description})
    (list "" @code{#f}        "Local time")
    (list "" @code{<integer>} "Seconds of offset from UTC. For example, New York City is -18000 
                              (-5 hours), San Francisco is -28800 (-8 hours).")
    (list "" @code{<string>}  "A POSIX time zone string understood by the OS (i.e.., the sort of 
                              time zone assigned to the $TZ environment variable)."))]

An integer timezone gives the number of seconds you must add to UTC to get time in that zone. It 
is not "seconds west" of UTC -- that flips the sign.

A string timezone must follow the POSIX format for @code{$TZ} environment variable, as described 
@hyperlink["https://www.man7.org/linux/man-pages/man3/tzset.3.html"]{here}. Specifcally, it must follow 
one of two formats:
@itemlist[
  @item{It can be a filepath to a @code{tzfile}-format file that contains full timezone information.
        If the path does not begin with a "/", the path is understood relative to the system timezone
        directory (usually @code{/usr/share/zoneinfo}). Examples are "EST5EDT" or "Asia/Vladivostok".}
  @item{It can be a string of characters that directly represent the timezone to be used; the specifics
        of string format can be found in man pages. Note that a string of this format does not 
        correspond to any of existing @code{tzfile}s: for examaple, "EDT4" is valid (timezone called 
        "EDT" that is 4 hours behind UTC), but it gives no information about summer time changes we
        expect in the "EST5EDT" timezone. An example of a complete timezone spec string is 
        @verbatim{NZST-12:00:00NZDT-13:00:00,M9.5.0,M4.1.0/3}}]

To get UTC time, use a time zone of either 0 or "UCT0".

@section{Procedures}

@deftogether[(@defproc[(time+ticks) (values [secs ticks])]
              @defproc[(ticks/sec) (values real number)])]{
  @code{time+ticks} gives the current time, with sub-second resolution. 
  Sub-second resolution is not provided by POSIX, but is available on many systems. 
  The time is returned as elapsed seconds since the Unix epoch, plus a number of sub-second "ticks". 
  The length of a tick may vary from implementation to implementation; it can be determined from 
  @code{(ticks/sec)}.

  The system clock is not required to report time at the full resolution given by @code{(ticks/sec)}. 
  For example, on BSD, time is reported at 1µs resolution, so @code{(ticks/sec)} is 1,000,000. 
  That doesn't mean the system clock has micro-second resolution.

  If the OS does not support sub-second resolution, the ticks value is always 0, and @code{(ticks/sec)} 
  returns 1.

  Remark: We chose to represent system clock resolution as ticks/sec instead of sec/tick to increase 
  the odds that the value could be represented as an exact integer, increasing efficiency and making 
  it easier for Scheme implementations that don't have sophisticated numeric support to deal with the 
  quantity.

  You can convert seconds and ticks to seconds with the expression:
  @codeblock{
    (+ secs (/ ticks (ticks/sec)))  
  }
}

@margin-note{
  The Posix facility for converting dates to times, @code{mktime()}, has a broken design: it indicates
  an error by returning -1, which is also a legal return value (for date 23:59:59 UCT, 12/31/1969).
  Scsh resolves the ambiguity in a paranoid fashion: it always reports an error if the underlying 
  Unix facility returns -1. We feel your pain. 
}
@defproc[(date [time integer? (time)] [tz (or integer? string? #f) #f]) date-record]{

  Simple @code{(date)} returns the current date, in the local time zone. 

  With the optional arguments, @code{date} converts the @var{time} to the date as specified by the 
  time zone @var{tz}. @var{time} defaults to the current time as if obtained with @code{(time)}. 
  @var{tz} defaults to local time #f, and it must conform to the format described in the 
  @seclink["time-zones-sec"]{time zone section}.

  If the @var{tz} argument is an integer, the date's @code{tz-name} field is a POSIX time zone of the 
  form "UTC+hh:mm:ss"; the trailing :mm:ss portion is deleted if it is zeroes.
}

@defproc*[([(time) integer]
           [(time [date date?]) integer])]{
  Simple @code{(time)} returns the current time with @code{time(NULL)}. With the optional @var{date} 
  argument, time converts a @code{date} record to a corresponding time.

  Note that the input @code{date} record is overconstrained. @code{time} ignores @var{date}'s 
  @code{week-day} and @code{year-day} fields. 
  
  If the date's @code{tz-secs} field is set, the @code{tz-name} and @code{summer?} fields are ignored.
  If the @code{tz-secs} field is #f, then the time zone information is taken from the @code{tz-name} 
  field. A false @code{tz-name} means the system's current time zone. 
  
  When calculating with time zones, the date's @code{summer?} field is used to resolve ambiguities.
  If it is #f, resolve ambiguous time in favor of non-summer time. Otherwise, if #t, resolve an 
  ambiguous time in favor of summer time.

  This is useful in boundary cases during the change-over. For example, in the Fall, when US daylight 
  savings time changes over at 2:00 am, 1:30 am happens twice -- it names two instants in time, an 
  hour apart.

  Outside of these boundary cases, the @code{summer?} flag is ignored. For example, if the 
  standard/summer change-overs happen in the Fall and the Spring, then the value of @code{summer?}
  is ignored for a January or July date. A January date would be resolved with standard time, 
  and a July date with summer time, regardless of the @code{summer?} value.

  The @code{summer?} flag is also ignored if the time zone doesn't have a summer time -- for example, 
  simple UTC.
}

@deftogether[(@defproc[(date->string [date date?]) string]
              @defproc[(format-date [fmt string?] [date date?]) string])]{
  @code{date->string} formats the @var{date} as a 24-character string of the form:
  @verbatim{Sun Sep 16 01:03:52 1973} 

  @code{format-date} formats the @var{date} according to the format string @var{fmt}. The format 
  string is copied verbatim, except that tilde characters indicate conversion specifiers that are 
  replaced by fields from the @code{date} record. 
  
  Below is a full of conversion specifiers supported by @code{format-date}:
  @tabular[#:style 'boxed
          #:row-properties '((bottom-border top-border) '())
          #:sep @hspace[3]
  (list (list "" @bold{Conversion Specifier} @bold{Description})
    (list "" "~~"  "converted to the ~ character.")
    (list "" "~a"  "abbreviated weekday name")
    (list "" "~A"  "full weekday name")
    (list "" "~b"  "abbreviated month name")
    (list "" "~B"  "full month name")
    (list "" "~c"  "time and date using the time and date representation for the locale")
    (list "" "~d"  "day of the month as a decimal number (01-31)")
    (list "" "~H"  "hour based on a 24-hour clock as a decimal number (00-23)")
    (list "" "~I"  "hour based on a 12-hour clock as a decimal number (01-12)")
    (list "" "~j"  "day of the year as a decimal number (001-366)")
    (list "" "~m"  "month as a decimal number (01-12)")
    (list "" "~M"  "minute as a decimal number (00-59)")
    (list "" "~p"  "AM/PM designation associated with a 12-hour clock")
    (list "" "~S"  "second as a decimal number (00-61)")
    (list "" "~U"  "week number of the year; Sunday is first day of week (00-53)")
    (list "" "~w"  "weekday as a decimal number (0-6), where Sunday is 0")
    (list "" "~W"  "week number of the year; Monday is first day of week (00-53)")
    (list "" "~x"  "date using the date representation for the locale")
    (list "" "~X"  "time using the time representation for the locale")
    (list "" "~y"  "year without century (00-99)")
    (list "" "~Y"  "year with century (e.g.1990)")
    (list "" "~Z"  "time zone name or abbreviation, or no characters if no time zone is determinable"))]
}    