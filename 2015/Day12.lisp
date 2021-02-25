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

(defparameter *json-input* "~/quicklisp/local-projects/rich/advent/2015/Day12.json")
(defparameter *test-input2* "~/quicklisp/local-projects/rich/advent/2015/sample2.json")
(defparameter *test-input3* "~/quicklisp/local-projects/rich/advent/2015/sample3.json")

;; Json-decode turns json arrays ([..]) into lists.  It turns json objects ({..})
;; into JSON-OBJECTS.

(defun create-json-object (file)
  (with-open-file (stream file)
    (let ((json-text (read-line stream nil nil)))
      (json-decode json-text))))

(defun json-obj-p (object)
  (equal 'json-object (type-of object)))

;; My approach here does not produce the correct answer.  This is because I understood
;; the rules to be that nested objects affect the entire set of objects in the nested set.
;; (e.g. {"foo":5, "bar":{"red":3}} I thought should return 0.)  As it turns out, the
;; rules mean to ignore only the object that directly contains "red".  Therefore, my
;; example should actually result in 5.

(defun sum-arrays (arr)
  (cond ((null arr) 0)
        ((numberp arr) arr)
        ((listp arr) (+ (sum-arrays (car arr)) (sum-arrays (cdr arr))))
        ((json-obj-p arr) (sum-object arr))
        (t 0)))

(defun sum-object (object)
  (let ((members (json-object-members object)))
    (multiple-value-bind (sum red-found) (sum-object-members members)
      (if red-found
          0
          sum))))

(defun sum-object-members (lst &optional (red-found nil))
  (cond ((null lst) (values 0 red-found))
        ((or red-found (equal lst "red")) (values 0 t))
        ((numberp lst) (values lst red-found))
        ((atom lst) (values 0 red-found))

        ((listp (first lst)) (multiple-value-bind (sum1 red1) (sum-object-members (car lst))
                               (multiple-value-bind (sum2 red2) (sum-object-members (cdr lst))
                                 (values (+ sum1 sum2) (or red1 red2)))))

        ((listp (second lst)) (multiple-value-bind (sum red) (sum-object-members (first lst) red-found)
                                (values (+ sum (sum-arrays (second lst))) red)))

        ((json-obj-p (second lst)) (let ((members (json-object-members (second lst))))
                                     (multiple-value-bind (sum1 red1) (sum-object-members (first lst) red-found)
                                       (multiple-value-bind (sum2 red2) (sum-object-members members red-found)
                                         (values (+ sum1 sum2) (or red1 red2))))))

        ((listp lst) (multiple-value-bind (sum1 red1) (sum-object-members (first lst) red-found)
                       (multiple-value-bind (sum2 red2) (sum-object-members (second lst) red-found)
                         (values (+ sum1 sum2) (or red1 red2)))))))

(sum-arrays (create-json-object *json-input*))

;; Mimicing an algorithm posted on Reddit

;; This solution does produce the correct answer. And it demonstrated to me what the
;; intent of the instructions were. It's hard to solve a problem with the rules are
;; not clearly stated.

(defun sumjson (object)
  (cond ((numberp object) object)
        ((json-obj-p object) (sumjson-dict object))
        ((listp object) (sumjson-list object))
        (t 0)))

(defun sumjson-dict (dict)
  (let ((dict-members (json-object-members dict)))
    (loop :for v :in dict-members
          :if (member "red" v :test 'equal)
            :return 0
          :sum (sumjson v))))

(defun sumjson-list (lst)
  (loop :for v :in lst
        :sum (sumjson v)))

(sumjson (create-json-object *json-input*))
