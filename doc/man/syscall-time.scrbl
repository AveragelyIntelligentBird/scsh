#lang scribble/doc
@(require scribble/manual "def-with-nolink.rkt")

@title{Time}


@section{Terminology}
@section{Basic data types}
@section{Time zones}

<speak about timezone format + link this: https://www.man7.org/linux/man-pages/man3/tzset.3.html>

@section{Procedures}

 Scsh's time system is fairly sophisticated, particularly with respect to its careful treatment of time zones. However, casual users shouldn't be intimidated; all of the complexity is optional, and defaulting all the optional arguments reduces the system to a simple interface.

3.10.1  Terminology

``UTC'' and ``UCT'' stand for ``universal coordinated time,'' which is the official name for what is colloquially referred to as ``Greenwich Mean Time.''

POSIX allows a single time zone to specify two different offsets from UTC: one standard one, and one for ``summer time.'' Summer time is frequently some sort of daylight savings time.

The scsh time package consistently uses this terminology: we never say ``gmt'' or ``dst;'' we always say ``utc'' and ``summer time.''

3.10.2  Basic data types

We have two types: time and date.

A time specifies an instant in the history of the universe. It is location and time-zone independent.8 A time is a real value giving the number of elapsed seconds since the Unix ``epoch'' (Midnight, January 1, 1970 UTC). Time values provide arbitrary time resolution, limited only by the number system of the underlying Scheme system.

A date is a name for an instant in time that is specified relative to some location/time-zone in the world, e.g.:

    Friday October 31, 1994 3:47:21 pm EST. 

Dates provide one-second resolution, and are expressed with the following record type:


    (define-record date     ; A Posix tm struct
      seconds       ; Seconds after the minute [0-59]
      minute        ; Minutes after the hour [0-59]
      hour          ; Hours since midnight [0-23]
      month-day     ; Day of the month [1-31]
      month         ; Months since January [0-11]
      year          ; Years since 1900
      tz-name       ; Time-zone name: #f or a string.
      tz-secs       ; Time-zone offset: #f or an integer.
      summer?       ; Summer (Daylight Savings) time in effect?
      week-day      ; Days since Sunday [0-6]
      year-day)     ; Days since Jan. 1 [0-365]

If the tz-secs field is given, it specifies the time-zone's offset from UTC in seconds. If it is specified, the tz-name and summer? fields are ignored when using the date structure to determine a specific instant in time.

If the tz-name field is given, it is a time-zone string such as "EST" or "HKT" understood by the OS. Since POSIX time-zone strings can specify dual standard/summer time-zones (e.g., "EST5EDT" specifies U.S. Eastern Standard/Eastern Daylight Time), the value of the summer? field is used to resolve the amiguous boundary cases. For example, on the morning of the Fall daylight savings change-over, 1:00am-2:00am happens twice. Hence the date 1:30 am on this morning can specify two different seconds; the summer? flag says which one.

A date with tz-name = tz-secs = #f is a date that is specified in terms of the system's current time zone.

There is redundancy in the date data structure. For example, the year-day field is redundant with the month-day and month fields. Either of these implies the values of the week-day field. The summer? and tz-name fields are redundant with the tz-secs field in terms of specifying an instant in time. This redundancy is provided because consumers of dates may want it broken out in different ways. The scsh procedures that produce date records fill them out completely. However, when date records produced by the programmer are passed to scsh procedures, the redundancy is resolved by ignoring some of the secondary fields. This is described for each procedure below.

(make-date s min h mday mon y [tzn tzs summ? wday yday])     --->     date         (procedure) 

    When making a date record, the last five elements of the record are optional, and default to #f, #f, #f, 0, and 0 respectively. This is useful when creating a date record to pass as an argument to time. Other procedures, however, may refuse to work with these incomplete date records. 

3.10.3  Time zones

Several time procedures take time zones as arguments. When optional, the time zone defaults to local time zone. Otherwise the time zone can be one of:

    #f 	Local time
    Integer 	Seconds of offset from UTC. For example, New York City is -18000 (-5 hours), San Francisco is -28800 (-8 hours).
    String 	A POSIX time zone string understood by the OS (i.e.., the sort of time zone assigned to the $TZ environment variable).

An integer time zone gives the number of seconds you must add to UTC to get time in that zone. It is not ``seconds west'' of UTC -- that flips the sign.

To get UTC time, use a time zone of either 0 or "UCT0".

3.10.4  Procedures

(time+ticks)     --->     [secs ticks]         (procedure) 
(ticks/sec)     --->     real         (procedure) 

    The current time, with sub-second resolution. Sub-second resolution is not provided by POSIX, but is available on many systems. The time is returned as elapsed seconds since the Unix epoch, plus a number of sub-second ``ticks.'' The length of a tick may vary from implementation to implementation; it can be determined from (ticks/sec).

    The system clock is not required to report time at the full resolution given by (ticks/sec). For example, on BSD, time is reported at 1µs resolution, so (ticks/sec) is 1,000,000. That doesn't mean the system clock has micro-second resolution.

    If the OS does not support sub-second resolution, the ticks value is always 0, and (ticks/sec) returns 1.

        Remark: I chose to represent system clock resolution as ticks/sec instead of sec/tick to increase the odds that the value could be represented as an exact integer, increasing efficiency and making it easier for Scheme implementations that don't have sophisticated numeric support to deal with the quantity.

        You can convert seconds and ticks to seconds with the expression

            (+ secs (/ ticks (ticks/sec)))

        Given that, why not have the fine-grain time procedure just return a non-integer real for time? Following Common Lisp, I chose to allow the system clock to report sub-second time in its own units to lower the overhead of determining the time. This would be important for a system that wanted to precisely time the duration of some event. Time stamps could be collected with little overhead, deferring the overhead of precisely calculating with them until after collection.

        This is all a bit academic for the Scheme 48 implementation, where we determine time with a heavyweight system call, but it's nice to plan for the future.

(date)     --->     date-record         (procedure) 
(date [time tz])     --->     date-record         (procedure) 

    Simple (date) returns the current date, in the local time zone.

    With the optional arguments, date converts the time to the date as specified by the time zone tz. Time defaults to the current time; tz defaults to local time, and is as described in the time-zone section.

    If the tz argument is an integer, the date's tz-name field is a POSIX time zone of the form ``UTC+hh:mm:ss''; the trailing :mm:ss portion is deleted if it is zeroes.

        Oops: The Posix facility for converting dates to times, mktime(), has a broken design: it indicates an error by returning -1, which is also a legal return value (for date 23:59:59 UCT, 12/31/1969). Scsh resolves the ambiguity in a paranoid fashion: it always reports an error if the underlying Unix facility returns -1. We feel your pain. 

(time)     --->     integer         (procedure) 
(time [date])     --->     integer         (procedure) 

    Simple (time) returns the current time.

    With the optional date argument, time converts a date to a time. Date defaults to the current date.

    Note that the input date record is overconstrained. time ignores date's week-day and year-day fields. If the date's tz-secs field is set, the tz-name and summer? fields are ignored.

    If the tz-secs field is #f, then the time-zone is taken from the tz-name field. A false tz-name means the system's current time zone. When calculating with time-zones, the date's summer? field is used to resolve ambiguities:

        #f 	Resolve an ambiguous time in favor of non-summer time.
        true 	Resolve an ambiguous time in favor of summer time.

    This is useful in boundary cases during the change-over. For example, in the Fall, when US daylight savings time changes over at 2:00 am, 1:30 am happens twice -- it names two instants in time, an hour apart.

    Outside of these boundary cases, the summer? flag is ignored. For example, if the standard/summer change-overs happen in the Fall and the Spring, then the value of summer? is ignored for a January or July date. A January date would be resolved with standard time, and a July date with summer time, regardless of the summer? value.

    The summer? flag is also ignored if the time zone doesn't have a summer time -- for example, simple UTC.

(date->string date)     --->     string         (procedure) 
(format-date fmt date)     --->     string         (procedure) 

    Date->string formats the date as a 24-character string of the form:

        Sun Sep 16 01:03:52 1973 

    Format-date formats the date according to the format string fmt. The format string is copied verbatim, except that tilde characters indicate conversion specifiers that are replaced by fields from the date record. Figure 1 gives the full set of conversion specifiers supported by format-date.

    ~~ 	Converted to the ~ character.
    ~a 	abbreviated weekday name
    ~A 	full weekday name
    ~b 	abbreviated month name
    ~B 	full month name
    ~c 	time and date using the time and date representation for the locale (~X ~x)
    ~d 	day of the month as a decimal number (01-31)
    ~H 	hour based on a 24-hour clock as a decimal number (00-23)
    ~I 	hour based on a 12-hour clock as a decimal number (01-12)
    ~j 	day of the year as a decimal number (001-366)
    ~m 	month as a decimal number (01-12)
    ~M 	minute as a decimal number (00-59)
    ~p 	AM/PM designation associated with a 12-hour clock
    ~S 	second as a decimal number (00-61)
    ~U 	week number of the year; Sunday is first day of week (00-53)
    ~w 	weekday as a decimal number (0-6), where Sunday is 0
    ~W 	week number of the year; Monday is first day of week (00-53)
    ~x 	date using the date representation for the locale
    ~X 	time using the time representation for the locale
    ~y 	year without century (00-99)
    ~Y 	year with century (e.g.1990)
    ~Z 	time zone name or abbreviation, or no characters if no time zone is determinable

    Figure 1:  format-date conversion specifiers

(fill-in-date! date)     --->     date         (procedure) 

    This procedure fills in missing, redundant slots in a date record. In decreasing order of priority:

        year, month, month-day ==> year-day
        If the year, month, and month-day fields are all defined (are all integers), the year-day field is set to the corresponding value.

        year, year-day ==> month, month-day
        If the month and month-day fields aren't set, but the year and year-day fields are set, then month and month-day are calculated.

        year, month, month-day, year-day ==> week-day
        If either of the above rules is able to determine what day it is, the week-day field is then set.

        tz-secs ==> tz-name
        If tz-secs is defined, but tz-name is not, it is assigned a time-zone name of the form ``UTC+hh:mm:ss''; the trailing :mm:ss portion is deleted if it is zeroes.

        tz-name, date, summer? ==> tz-secs, summer?
        If the date information is provided up to second resolution, tz-name is also provided, and tz-secs is not set, then tz-secs and summer? are set to their correct values. Summer-time ambiguities are resolved using the original value of summer?. If the time zone doesn't have a summer time variant, then summer? is set to #f.

        local time, date, summer? ==> tz-name, tz-secs, summer?
        If the date information is provided up to second resolution, but no time zone information is provided (both tz-name and tz-secs aren't set), then we proceed as in the above case, except the system's current time zone is used.

    These rules allow one particular ambiguity to escape: if both tz-name and tz-secs are set, they are not brought into agreement. It isn't clear how to do this, nor is it clear which one should take precedence.

        Oops: fill-in-date! isn't implemented yet.