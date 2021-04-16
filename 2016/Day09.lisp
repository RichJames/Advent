;;;;
;;;;  Advent of Code 2016
;;;;     Day 09

(defpackage #:2016Day09
  (:use #:cl #:cl-ppcre))

(in-package #:2016Day09)

(defparameter *marker-regex* "\\(\\d+x\\d+\\)")

(defun next-marker (string)
  (ppcre:scan *marker-regex* string))

(defun parse-marker (marker)
  "Parses a marker of form '(aaaxbbb)', returning aaaa and bbb."
  (let* ((x-delim         (position #\x marker))
         (chars-to-grab   (parse-integer (subseq marker 1 x-delim) :junk-allowed t))
         (repeat          (parse-integer (subseq marker (1+ x-delim) (1- (length marker))) :junk-allowed t)))
    (values chars-to-grab repeat)))

(defun decode-one-marker (string)
  (if (next-marker string)
      (multiple-value-bind (start end) (next-marker string)
        (let ((first     (subseq string 0 start))
              (marker    (subseq string start end))
              (rest      (subseq string end)))
          (multiple-value-bind (chars repeat) (parse-marker marker)
            (let ((repeat-chars  (subseq rest 0 chars))
                  (rest2         (subseq rest chars)))
              (loop :with result = first
                    :repeat repeat
                    :do (setf result (concatenate 'string result repeat-chars))
                    :finally (return (values result (if (= 0 (length rest2)) nil rest2))))))))
      (values string nil)))

(defun decode-string (string)
  (loop :with result = nil
        :for to-decode = string :then rest
        :for (decoded-part rest) = (multiple-value-list (decode-one-marker to-decode))
        :while rest
        :do (setf result (concatenate 'string result decoded-part))
        :finally (return (concatenate 'string result decoded-part))))

(defun part1 ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day09.txt")
    (length (decode-string (read-line stream nil nil)))))

;;; ***** Part 2 *****

(defun decoded-length (message)
  (multiple-value-bind (s e) (next-marker message)
    (if (and s e)
        (let* ((before (subseq message 0 s))
               (marker (subseq message s e)))
          (multiple-value-bind (chars repeat) (parse-marker marker)
            (let* ((chars-subseq  (subseq message e (+ e chars)))
                   (remain        (subseq message (+ e chars))))
              (+ (length before) (* repeat (decoded-length chars-subseq)) (decoded-length remain)))))
        (length message))))

(defun part2 ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day09.txt")
    (decoded-length (read-line stream nil nil))))
