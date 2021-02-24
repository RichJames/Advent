;;;;
;;;; Advent of Code 2015
;;;;   Day 12

(defpackage #:Day12
  (:use #:cl
        #:json))

(in-package #:Day12)

;;; Part 1
;;;  Find all the numbers in the json document provided and add them together.
;;;  The file is Day12.json.

(defun get-numbers ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2015/Day12.json")
    (let ((json-text (read-line stream nil nil)))
      (all-matches-as-strings "\-?\\d+" json-text))))

(defun sum-numbers (numbers)
  (reduce #'+ (mapcar #'parse-integer numbers)))

;;; Part 2
;;;  Ignore any object (and all of its children) which has any property with the
;;;  value "red".  Do this only for objects ({...}), not arrays ([...]).

;;; Use regex-replace-all to replace all objects with "red" with just "red" and
;;; iterate calls to regex-replace-all until the string is no longer changed.
;;; Then do the part1 search and sum again.

(defun remove-red (input)
  (with-open-file (stream input)
    (let ((json-text (read-line stream nil nil)))
      (regex-replace-all "\{[^\{\}]*\"red\"[^\{\}]*\}" json-text "\"red\""))))

(defun get-part2-numbers ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2015/Day12.json")
    (let ((json-text (read-line stream nil nil)))
      (all-matches-as-strings "\-?\\d+" (remove-all-red json-text)))))

(defun remove-all-red (input)
  (let ((new-string nil)
        (changed nil))
    (loop :do (setf (values new-string changed) (remove-red input))
          :while changed
          :do (setf input new-string)
          :finally (return new-string))))

(defparameter *json-input* "~/quicklisp/local-projects/rich/advent/2015/Day12.json")
(defparameter *test-input* "~/quicklisp/local-projects/rich/advent/2015/sample2.json")

(defun get-part2-numbers (file)
  (with-open-file (stream file)
    (let ((json-text (read-line stream nil nil)))
      (remove-red json-text))))

;; Json-decode turns json arrays ([..]) into lists.  It turns json objects ({..})
;; into JSON-OBJECTS.

(defun create-json-object (file)
  (with-open-file (stream file)
    (let ((json-text (read-line stream nil nil)))
      (json-decode json-text))))

;; object members can be a nested list of lists

(defun sum-arrays (arr)
  (cond ((null arr) 0)
        ((listp arr) (+ (sum-arrays (car arr)) (sum-arrays (cdr arr))))
        ((numberp arr) arr)
        ((equal 'json-object (type-of arr)) (sum-object arr))
        (t 0)))

(defun sum-object (object)
  (multiple-value-bind (sum red-found) (sum-object-members object)
    (if red-found
        0
        sum)))

(defun sum-object-members (lst &optional (red-found nil))
  (cond (red-found (values 0 t))
        ((null lst) (values 0 red-found))
        ((listp lst) (multiple-value-bind (sum1 red1) (sum-object-members (car lst) red-found)
                       (multiple-value-bind (sum2 red2) (sum-object-members (cdr lst) red-found)
                         (if (or red1 red2)
                             (values 0 t)
                             (values (+ sum1 sum2) nil)))))
        ((numberp lst) (values lst red-found))
        ((equal 'json-object (type-of lst)) (let ((members (json-object-members lst)))
                                              (multiple-value-bind (sum red) (sum-object-members members red-found)
                                                (values sum red))))
        ((and (stringp lst) (string= lst "red")) (values 0 t))
        (t (values 0 red-found))))
