;;; Date & Time Tests ---------------------------------------------------------
;; Part of scsh 1.0. See file COPYING for notices and license.
;; Tests for scsh's date and time fascilities   

;;; Test data -----------------------------------------------------------------
;; Defines records & constants used throughout the module

(define td-seconds 13)     ; Seconds after the minute [0-59]
(define td-minute 4)       ; Minutes after the hour [0-59]
(define td-hour 0)         ; Hours since midnight [0-23]
(define td-month-day 12)   ; Day of the month [1-31]
(define td-month 5)        ; Months since January [0-11]
(define td-year 109)       ; Years since 1900
(define td-tz-name "UTC")  ; Time-zone name: #f or a string.
(define td-tz-secs 0)      ; Time-zone offset: #f or an integer.
(define td-summer? #f)     ; Summer (Daylight Savings) time in effect?
(define td-week-day 5)     ; Days since Sunday [0-6]
(define td-year-day 162)   ; Days since Jan. 1 [0-365]
(define td-date (make-date 
                    td-seconds 
                    td-minute 
                    td-hour
                    td-month-day 
                    td-month 
                    td-year 
                    td-tz-name 
                    td-tz-secs 
                    td-summer? 
                    td-week-day 
                    td-year-day))
(define td-date-min (make-date 
                    td-seconds 
                    td-minute 
                    td-hour
                    td-month-day 
                    td-month 
                    td-year))
(define td-time 1244765053) 

(define td-fun-tz-name "EST5EDT") ; Time-zone name: #f or a string.
(define td-fun-tz-secs -14400)    ; Time-zone offset: #f or an integer.
(define td-fun-summer? #t)        ; Summer (Daylight Savings) time in effect?
(define td-fun-date (make-date 
                    td-seconds 
                    td-minute 
                    td-hour
                    td-month-day 
                    td-month 
                    td-year 
                    td-fun-tz-name ; <-
                    td-fun-tz-secs ; <-
                    td-fun-summer? ; <-
                    td-week-day 
                    td-year-day))
(define td-fun-time 1244779453)   ; NOTE: corresponds to td-fun-date in EDT+4 (i.e. UTC-4)

;;; Test code -----------------------------------------------------------------
;; All procedures return either #t or #f

;; (make-date s min h mday mon y [tzn tzs summ? wday yday])
(define (make-min-date-test)
     (and (= td-seconds   (date:seconds td-date-min))
	     (= td-minute    (date:minute td-date-min))
	     (= td-hour      (date:hour td-date-min))
	     (= td-month-day (date:month-day td-date-min))
	     (= td-month     (date:month td-date-min))
          (= td-year      (date:year td-date-min))
          (equal? #f      (date:tz-name td-date-min))
          (equal? #f      (date:tz-secs td-date-min))
          (equal? #f      (date:summer? td-date-min))
          (= 0            (date:week-day td-date-min))
          (= 0            (date:year-day td-date-min))))
(add-test! 'make-min-date  'datetime  make-min-date-test)

(define (make-full-date-test)
     (and (= td-seconds   (date:seconds td-date))
	     (= td-minute    (date:minute td-date))
	     (= td-hour      (date:hour td-date))
	     (= td-month-day (date:month-day td-date))
	     (= td-month     (date:month td-date))
          (= td-year      (date:year td-date))
          (equal? td-tz-name   (date:tz-name td-date))
          (equal? td-tz-secs   (date:tz-secs td-date))
          (equal? td-summer?   (date:summer? td-date))
          (= td-week-day  (date:week-day td-date))
          (= td-year-day  (date:year-day td-date))))
(add-test! 'make-full-date 'datetime  make-full-date-test)

;; Helper for comparing two date records
(define (dates-equal? d1 d2)
     (and (= (date:seconds d1)   (date:seconds d2))
	     (= (date:minute d1)    (date:minute d2))
	     (= (date:hour d1)      (date:hour d2))
	     (= (date:month-day d1) (date:month-day d2))
	     (= (date:month d1)     (date:month d2))
          (= (date:year d1)      (date:year d2))
          ; Note that we don't assert equality of tz-name because the same 
          ; timezone can have different valid names (i.e. EST5EDT and EDT+4)
          (equal? (date:tz-secs d1)   (date:tz-secs d2))
          (equal? (date:summer? d1)   (date:summer? d2))
          (= (date:week-day d1)  (date:week-day d2))
          (= (date:year-day d1)  (date:year-day d2))))

;; (date [time tz])
(define (date-of-time-test test-time test-tz-name ref-date)
     (dates-equal? (date test-time test-tz-name) ref-date))

(add-test! 'date-of-time-utc 'datetime  date-of-time-test 
     td-time td-tz-name td-date)
(add-test! 'date-of-time-fun 'datetime  date-of-time-test 
     td-fun-time td-fun-tz-name td-fun-date)

;; (time [date])
(define (time-of-date-test test-date ref-time) 
     (= (time test-date) ref-time))
     
(add-test! 'time-of-date-utc 'datetime  time-of-date-test 
     td-date td-time)
(add-test! 'time-of-date-fun 'datetime  time-of-date-test 
     td-fun-date td-fun-time)

;; (date->string date) & primitive timezone conversions
(define (date->string-test test-time test-tz-name expected-str) 
     (equal? (date->string (date test-time test-tz-name)) expected-str))

(add-test! 'date->string-utc 'datetime date->string-test 
     td-time td-tz-name "Fri Jun 12 00:04:13 2009")
(add-test! 'date->string-utc-in-edt 'datetime date->string-test 
     td-time td-fun-tz-name "Thu Jun 11 20:04:13 2009") ; UTC-4
(add-test! 'date->string-utc-in-vlat 'datetime date->string-test 
     td-time "VLAT-10" "Fri Jun 12 10:04:13 2009")      ; UTC+10

(add-test! 'date->string-edt 'datetime date->string-test 
     td-fun-time td-fun-tz-name "Fri Jun 12 00:04:13 2009")
(add-test! 'date->string-edt-in-utc 'datetime date->string-test 
     td-fun-time td-tz-name "Fri Jun 12 04:04:13 2009")  ; EDT+4
(add-test! 'date->string-edt-in-vlat 'datetime date->string-test 
     td-fun-time "VLAT-10" "Fri Jun 12 14:04:13 2009")  ; EDT+14 (i.e. UTC+10)
(add-test! 'date->string-edt-in-pst 'datetime date->string-test 
     td-fun-time "PST+8" "Thu Jun 11 20:04:13 2009")    ; EDT-4 (i.e. UTC-8)

;; (format-date fmt date) 
(define (format-date-test test-time test-tz-name expected-str)
     (equal? (format-date "~~ ~a ~A ~b ~B ~c ~d ~H ~I ~j ~m ~M ~p ~S ~U ~w ~W ~x ~X ~y ~Y ~Z" 
               (date test-time test-tz-name))
          expected-str))

(add-test! 'format-date-test-utc 'datetime format-date-test
     td-time td-tz-name 
     "~ Fri Friday Jun June Fri Jun 12 00:04:13 2009 12 00 12 163 06 04 AM 13 23 5 23 06/12/09 00:04:13 09 2009 UTC")
(add-test! 'format-date-test-utc-in-edt 'datetime format-date-test
     td-time td-fun-tz-name 
     "~ Thu Thursday Jun June Thu Jun 11 20:04:13 2009 11 20 08 162 06 04 PM 13 23 4 23 06/11/09 20:04:13 09 2009 EDT+4")
(add-test! 'format-date-test-edt 'datetime format-date-test
     td-fun-time td-fun-tz-name 
     "~ Fri Friday Jun June Fri Jun 12 00:04:13 2009 12 00 12 163 06 04 AM 13 23 5 23 06/12/09 00:04:13 09 2009 EDT+4")
(add-test! 'format-date-test-edt-in-utc 'datetime format-date-test
     td-fun-time td-tz-name 
     "~ Fri Friday Jun June Fri Jun 12 04:04:13 2009 12 04 04 163 06 04 AM 13 23 5 23 06/12/09 04:04:13 09 2009 UTC")