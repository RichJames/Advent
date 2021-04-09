;;;;
;;;;  Advent of Code 2016
;;;;     Day 04

(defpackage #:2016Day04
  (:use #:cl))

(in-package #:2016Day04)

(defstruct advent-room
  (letters nil)
  (id nil)
  (checksum nil))

(defun parse-encrypted-room-name (room)
  (let* ((last-dash     (position #\- room :from-end t))
         (room-letters  (remove #\- (subseq room 0 last-dash)))
         (first-bracket (position #\[ room))
         (room-id       (parse-integer (subseq room (1+ last-dash) first-bracket)))
         (room-checksum (subseq room (1+ first-bracket) (1- (length room)))))
    (make-advent-room
     :letters  room-letters
     :id       room-id
     :checksum room-checksum)))

(defun valid-checksum-p (room)
  (let ((letter-counts (make-array 26 :initial-element 0))
        (code-for-a    (char-code #\a)))

    (loop :for c :across (advent-room-letters room)
          :for c-code = (- (char-code c) code-for-a)
          :do (setf (aref letter-counts c-code) (1+ (aref letter-counts c-code))))

    (loop :for count :across letter-counts
          :for c-code = code-for-a :then (1+ c-code)
          :for letter = (code-char c-code) :then (code-char c-code)
          :if (> count 0)
            :collect (list letter count) :into results
          :finally (let ((ordered-letters (coerce (mapcar #'(lambda (x) (car x)) (stable-sort results #'> :key 'cadr)) 'string)))
                     (return (search (advent-room-checksum room) ordered-letters))))))

(defun part1 ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day04.txt")
    (loop :for r = (read-line stream nil nil)
          :while r
          :for parsed-r = (parse-encrypted-room-name r) :then (parse-encrypted-room-name r)
          :if (valid-checksum-p parsed-r)
            :collect (advent-room-id parsed-r) :into valid-room-ids
          :finally (return (reduce #'+ valid-room-ids)))))

;;; ***** Part2 *****

(defun shift-letter (c shift)
  (let* ((shift-amount (rem shift 26))
         (char-code-a  (char-code #\a))
         (c-code       (- (char-code c) char-code-a)))
    (if (equal c #\-)
        #\Space
        (code-char (+ (rem (+ c-code shift-amount) 26) char-code-a)))))

(defun decrypt-room-name (room-name shift)
  (loop :for c :across room-name
        :collect (shift-letter c shift) :into decryption-list
        :finally (return (coerce decryption-list 'string))))

(defun get-encrypted-name-and-id (room)
  (let* ((last-dash     (position #\- room :from-end t))
         (room-name     (subseq room 0 last-dash))
         (first-bracket (position #\[ room))
         (room-id       (parse-integer (subseq room (1+ last-dash) first-bracket))))
    (values room-name room-id)))

(defun part2 ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day04.txt")
    (loop :for r = (read-line stream nil nil)
          :while r
          :for parsed-r = (parse-encrypted-room-name r) :then (parse-encrypted-room-name r)
          :if (valid-checksum-p parsed-r)
            :collect (multiple-value-bind (name id) (get-encrypted-name-and-id r)
                       (list id (decrypt-room-name name id))) :into decrypted-rooms
          :finally (return (find-if #'(lambda (x) (search "north" (cadr x))) decrypted-rooms)))))
