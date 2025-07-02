;;; datetime-tests ------------------------------------------------------------
;; Part of scsh 1.0. See file COPYING for notices and license.
;; Tests for scsh's date and time fascilities   

;;; Test data -----------------------------------------------------------------
;; Defines records to be used throughout the module

(define ex-seconds 13)     ; Seconds after the minute [0-59]
(define ex-minute 4)       ; Minutes after the hour [0-59]
(define ex-hour 0)         ; Hours since midnight [0-23]
(define ex-month-day 12)   ; Day of the month [1-31]
(define ex-month 5)        ; Months since January [0-11]
(define ex-year 109)       ; Years since 1900
(define ex-tz-name "UTC")  ; Time-zone name: #f or a string.
(define ex-tz-secs 0)      ; Time-zone offset: #f or an integer.
(define ex-summer? #f)     ; Summer (Daylight Savings) time in effect?
(define ex-week-day 5)     ; Days since Sunday [0-6]
(define ex-year-day 162)   ; Days since Jan. 1 [0-365]
(define ex-date (make-date 
                    ex-seconds 
                    ex-minute 
                    ex-hour
                    ex-month-day 
                    ex-month 
                    ex-year 
                    ex-tz-name 
                    ex-tz-secs 
                    ex-summer? 
                    ex-week-day 
                    ex-year-day))
(define ex-date-min (make-date 
                    ex-seconds 
                    ex-minute 
                    ex-hour
                    ex-month-day 
                    ex-month 
                    ex-year))
(define ex-time 1244765053) 

(define ex-fun-tz-name "EST5EDT") ; Time-zone name: #f or a string.
(define ex-fun-tz-secs -14400)    ; Time-zone offset: #f or an integer.
(define ex-fun-summer? #t)        ; Summer (Daylight Savings) time in effect?
(define ex-fun-date (make-date 
                    ex-seconds 
                    ex-minute 
                    ex-hour
                    ex-month-day 
                    ex-month 
                    ex-year 
                    ex-fun-tz-name ; <-
                    ex-fun-tz-secs ; <-
                    ex-fun-summer? ; <-
                    ex-week-day 
                    ex-year-day))
(define ex-fun-time 1244779453)   ; NOTE: corresponds to ex-fun-date in EDT+4 (i.e. UTC-4)

;;; Test code -----------------------------------------------------------------
;; All procedures return either #t or #f

;; (make-date s min h mday mon y [tzn tzs summ? wday yday])
(define (make-min-date-test)
     (and (equal? ex-seconds   (date:seconds ex-date-min))
	     (equal? ex-minute    (date:minute ex-date-min))
	     (equal? ex-hour      (date:hour ex-date-min))
	     (equal? ex-month-day (date:month-day ex-date-min))
	     (equal? ex-month     (date:month ex-date-min))
          (equal? ex-year      (date:year ex-date-min))
          (equal? #f           (date:tz-name ex-date-min))
          (equal? #f           (date:tz-secs ex-date-min))
          (equal? #f           (date:summer? ex-date-min))
          (equal? 0            (date:week-day ex-date-min))
          (equal? 0            (date:year-day ex-date-min))))
(add-test! 'make-min-date  'datetime  make-min-date-test)

(define (make-full-date-test)
     (and (equal? ex-seconds   (date:seconds ex-date))
	     (equal? ex-minute    (date:minute ex-date))
	     (equal? ex-hour      (date:hour ex-date))
	     (equal? ex-month-day (date:month-day ex-date))
	     (equal? ex-month     (date:month ex-date))
          (equal? ex-year      (date:year ex-date))
          (equal? ex-tz-name   (date:tz-name ex-date))
          (equal? ex-tz-secs   (date:tz-secs ex-date))
          (equal? ex-summer?   (date:summer? ex-date))
          (equal? ex-week-day  (date:week-day ex-date))
          (equal? ex-year-day  (date:year-day ex-date))))
(add-test! 'make-full-date 'datetime  make-full-date-test)

;; (time+ticks) & (time)
(define (time+ticks-eq-time-test)
     (call-with-values (lambda () (time+ticks)) 
          (lambda (extracted-time ticks) (= extracted-time (time)))))
(add-test! 'time+ticks-eq-time 'datetime  time+ticks-eq-time-test)

;; Helper for comparing two date records

(define (dates-equal? d1 d2)
     (and (equal? (date:seconds d1)   (date:seconds d2))
	     (equal? (date:minute d1)    (date:minute d2))
	     (equal? (date:hour d1)      (date:hour d2))
	     (equal? (date:month-day d1) (date:month-day d2))
	     (equal? (date:month d1)     (date:month d2))
          (equal? (date:year d1)      (date:year d2))
          ; Note that we don't assert equality of tz-name because the same 
          ; timezone can have different valid names (i.e. EST5EDT and EDT+4)
          (equal? (date:tz-secs d1)   (date:tz-secs d2))
          (equal? (date:summer? d1)   (date:summer? d2))
          (equal? (date:week-day d1)  (date:week-day d2))
          (equal? (date:year-day d1)  (date:year-day d2))))

;; (date [time tz])
(define (date-of-time-test test-time test-tz-name ref-date)
     (dates-equal? (date test-time test-tz-name) ref-date))

(add-test! 'date-of-time-utc 'datetime  date-of-time-test 
     ex-time ex-tz-name ex-date)
(add-test! 'date-of-time-fun 'datetime  date-of-time-test 
     ex-fun-time ex-fun-tz-name ex-fun-date)

;; (time [date])
(define (time-of-date-test test-date ref-time) 
     (equal? (time test-date) ref-time))
     
(add-test! 'time-of-date-utc 'datetime  time-of-date-test 
     ex-date ex-time)
(add-test! 'time-of-date-fun 'datetime  time-of-date-test 
     ex-fun-date ex-fun-time)

;; (date->string date) & primitive timezone conversions
(define (date->string-test test-time test-tz-name expected-str) 
     (equal? (date->string (date test-time test-tz-name)) expected-str))

(add-test! 'date->string-utc 'datetime date->string-test 
     ex-time ex-tz-name "Fri Jun 12 00:04:13 2009")
(add-test! 'date->string-utc-in-edt 'datetime date->string-test 
     ex-time ex-fun-tz-name "Thu Jun 11 20:04:13 2009") ; UTC-4
(add-test! 'date->string-utc-in-vlat 'datetime date->string-test 
     ex-time "VLAT-10" "Fri Jun 12 10:04:13 2009")      ; UTC+10

(add-test! 'date->string-edt 'datetime date->string-test 
     ex-fun-time ex-fun-tz-name "Fri Jun 12 00:04:13 2009")
(add-test! 'date->string-edt-in-utc 'datetime date->string-test 
     ex-fun-time ex-tz-name "Fri Jun 12 04:04:13 2009")  ; EDT+4
(add-test! 'date->string-edt-in-vlat 'datetime date->string-test 
     ex-fun-time "VLAT-10" "Fri Jun 12 14:04:13 2009")  ; EDT+14 (i.e. UTC+10)
(add-test! 'date->string-edt-in-pst 'datetime date->string-test 
     ex-fun-time "PST+8" "Thu Jun 11 20:04:13 2009")    ; EDT-4 (i.e. UTC-8)

;; (format-date fmt date) 
(define (format-date-test test-time test-tz-name expected-str)
     (equal? (format-date "~~ ~a ~A ~b ~B ~c ~d ~H ~I ~j ~m ~M ~p ~S ~U ~w ~W ~x ~X ~y ~Y ~Z" 
               (date test-time test-tz-name))
          expected-str))

(add-test! 'format-date-test-utc 'datetime format-date-test
     ex-time ex-tz-name 
     "~ Fri Friday Jun June Fri Jun 12 00:04:13 2009 12 00 12 163 06 04 AM 13 23 5 23 06/12/09 00:04:13 09 2009 UTC")
(add-test! 'format-date-test-utc-in-edt 'datetime format-date-test
     ex-time ex-fun-tz-name 
     "~ Thu Thursday Jun June Thu Jun 11 20:04:13 2009 11 20 08 162 06 04 PM 13 23 4 23 06/11/09 20:04:13 09 2009 EDT+4")
(add-test! 'format-date-test-edt 'datetime format-date-test
     ex-fun-time ex-fun-tz-name 
     "~ Fri Friday Jun June Fri Jun 12 00:04:13 2009 12 00 12 163 06 04 AM 13 23 5 23 06/12/09 00:04:13 09 2009 EDT+4")
(add-test! 'format-date-test-edt-in-utc 'datetime format-date-test
     ex-fun-time ex-tz-name 
     "~ Fri Friday Jun June Fri Jun 12 04:04:13 2009 12 04 04 163 06 04 AM 13 23 5 23 06/12/09 04:04:13 09 2009 UTC")