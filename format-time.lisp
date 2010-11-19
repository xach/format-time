;;;
;;; Usage example:
;;;
;;;  (format-time nil "~{Dayname}, ~{Monthname} ~:@{Date}") =>
;;;     "Monday, May 24th"
;;;

(defpackage :format-time
  (:use :cl)
  (:export :format-time :time-formatter
           :iso-8601z))

(in-package :format-time)


;;; Errors

(define-condition format-time-error (error)
  ((complaint :reader format-time-error-complaint :initarg :complaint)
   (control-string :reader format-time-error-control-string
                   :initarg :control-string)
   (offset :reader format-time-error-offset :initarg :offset))
  (:report report-format-time-error))


(define-condition format-time-dumb-error (error)
  ((complaint :initarg :complaint :reader format-time-dumb-error-complaint))
  (:report (lambda (condition stream)
             (write-string (format-time-dumb-error-complaint condition)
                           stream))))


(defun report-format-time-error (condition stream)
  (format stream "error in time-format: ~A~%  ~A~%  ~v@T^~%"
          (format-time-error-complaint condition)
          (format-time-error-control-string condition)
          (format-time-error-offset condition)))



;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *day-of-week-names*
    #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

  (defvar *month-names*
    #("January" "February" "March" "April" "May" "June"
      "July" "August" "September" "October" "November" "December"))

  (defvar *number-suffixes*
    #("th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th"))

  (defvar *decoded-time-values*
    '(second minute hour date month year day daylight-p zone))

  (defvar *date-formatters* (make-hash-table :test 'eq)))


(defun number-suffix (number)
  (svref *number-suffixes* (mod number 10)))


(defmacro def-date-formatter (name (directive type) args &body body)
  "Create a format-time directive processor implemented with a
function called NAME that processes a directive of type TYPE (one of
:STRING OR :NUMERIC) named by DIRECTIVE. ARGS must only consist of
symbols in *DECODED-TIME-VALUES*."
  (assert (subsetp args *decoded-time-values*))
  (assert (member type '(:string :numeric)))
  (let ((ignore-list (set-difference *decoded-time-values* args)))
    (setf (gethash directive *date-formatters*) name)
    (setf (get directive 'strftime-type) type)
    `(defun ,name (,@*decoded-time-values*)
      (declare (ignore ,@ignore-list))
      ,@body)))


(def-date-formatter format-dayname (dayname :string) (day)
  (svref *day-of-week-names* day))

(def-date-formatter format-monthname (monthname :string) (month)
  (svref *month-names* (1- month)))

(def-date-formatter format-day (day :numeric) (day)
  day)

(def-date-formatter format-month (month :numeric) (month)
  month)

(def-date-formatter format-date (date :numeric) (date)
  date)

(def-date-formatter format-year (year :numeric) (year)
  year)

(def-date-formatter format-yy (yy :numeric) (year)
  (mod year 100))

(def-date-formatter format-hh (hh :numeric) (hour)
  hour)

(def-date-formatter format-hh12 (hh12 :numeric) (hour)
  (let ((12hour (mod hour 12)))
    (if (zerop 12hour)
        12
        12hour)))

(def-date-formatter format-mm (mm :numeric) (minute)
  minute)

(def-date-formatter format-ss (ss :numeric) (second)
  second)

(def-date-formatter format-am (am :string) (hour)
  (if (< hour 12) "am" "pm"))



;;; parsing the control string
(defun make-directive-printer (directive colonp atp)
  (ecase (get directive 'strftime-type :error)
    (:string
     (cond ((not (or colonp atp))
            (lambda (arg stream)
              (write-string arg stream)))
           ((not colonp)
            (lambda (arg stream)
              (write-string (string-upcase arg) stream)))
           ((not atp)
            (lambda (arg stream)
              (dotimes (i 3)
                (write-char (schar arg i) stream))))
           (t
            (lambda (arg stream)
              (dotimes (i 3)
                (write-char (char-upcase (schar arg i)) stream))))))
    (:numeric
     (cond ((not (or colonp atp))
            (lambda (arg stream)
              (princ arg stream)))
           ((not colonp)
            (lambda (arg stream)
              (format stream "~2,'0D" arg)))
           ((not atp)
            (lambda (arg stream)
              (format stream "~2,' D" arg)))
           (t
            (lambda (arg stream)
              (format stream "~D~A" arg (number-suffix arg))))))
    (:error
     (error 'format-time-dumb-error
            :complaint "unknown format-time directive"))))


(defun make-directive-function (name-string colonp atp)
  (let* ((name (intern (string-upcase name-string)
                       (find-package "FORMAT-TIME")))
         (result-func (gethash name *date-formatters*))
         (format-func (make-directive-printer name colonp atp)))
    (lambda (stream second minute hour date month year day daylight-p zone)
      (funcall format-func
               (funcall result-func second minute hour date month year day daylight-p zone)
               stream))))



(defun tokenize-control-string (string)
  "Convert the control string STRING to a list of constant strings and
printing functions. Signals FORMAT-TIME-ERROR if there is a problem
with the control string."
  (declare (string string))
  (let ((pos 0)
        (end (length string))
        (results nil))
    (loop
     (when (>= pos end)
       (return (nreverse results)))
     (let ((directive (position #\~ string :start pos)))
       (cond ((null directive)
              (push (if (zerop pos) string (subseq string pos)) results)
              (return (nreverse results)))
             (t
              (when (< pos directive)
                (push (subseq string pos directive) results))
              (multiple-value-bind (func new-pos)
                  (parse-directive string directive)
                (push func results)
                (setf pos new-pos))))))))


(defun parse-directive (string start)
  "Convert a single directive from STRING starting at START into a
processing function."
  (declare (string string) (fixnum start))
  (let ((colonp nil)
        (atp nil)
        (posn (1+ start))
        (end (length string)))
    (labels ((pos-error (message &optional (pos posn))
               (error 'format-time-error
                      :complaint message
                      :control-string string
                      :offset (1- pos)))
             (get-char ()
               (if (= posn end)
                   (pos-error "string ended before directive was found")
                 (prog1 
                     (schar string posn)
                   (incf posn)))))
      (loop
       (let ((char (get-char)))
         (case char
           ((#\:)
            (if colonp
                (pos-error "too many colons supplied")
                (setf colonp t)))
           ((#\@)
            (if atp
                (pos-error "too many at-signs supplied")
                (setf atp t)))
           ((#\{)
            (let ((name-end (position #\} string :start posn)))
              (if name-end
                  (return
                    (values (handler-case
                                (make-directive-function
                                 (subseq string posn name-end) colonp atp)
                              (format-time-dumb-error (condition)
                                (pos-error (format-time-dumb-error-complaint condition) (1+ posn))))

                            (1+ name-end)))
                  (pos-error "no corresponding close brace"))))))))))


;;; user interface

(defun %format-time (stream control-string tz time)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time time tz)
    (dolist (item (tokenize-control-string control-string))
      (etypecase item
        (string (write-string item stream))
        (function (funcall item stream
                           second minute hour date month year day daylight-p zone))))))

                                       

(defun format-time (stream-designator control-string
                    &key tz (time (get-universal-time)))
  "Format a universal time into a human-readable time."
  (etypecase stream-designator
    (null
     (with-output-to-string (stream)
       (%format-time stream control-string tz time)))
    (string
     (with-output-to-string (stream stream-designator)
       (%format-time stream control-string tz time)))
    ((member t)
     (%format-time *standard-output* control-string tz time))
    (stream
     (%format-time stream-designator control-string tz time))))


(defun time-formatter-form (item)
  (if (stringp item)
      `(write-string ,item)
      `(funcall ,item *standard-output*
        second minute hour date month year day daylight-p zone)))


(defun %time-formatter-body (control-string)
  (let* ((items (tokenize-control-string control-string))
         (forms (mapcar #'time-formatter-form items)))
    `(lambda (*standard-output* &key (tz nil) (time (get-universal-time)))
      ,@(if (every #'stringp items)
            `((declare (ignore tz time))
              ,@forms)
            `((multiple-value-bind
                    (second minute hour date month year day daylight-p zone)
                  (decode-universal-time time tz)
                ,@forms))))))
        

(defmacro time-formatter (control-string)
  `#',(%time-formatter-body control-string))


;;; Some convenience functions

(defun iso-8601z (stream &optional time)
  (format-time:format-time stream
                           "~@{Year}-~@{Month}-~@{Date}T~@{HH}:~@{MM}:~@{SS}Z"
                           :tz 0
                           :time (or time (get-universal-time))))
