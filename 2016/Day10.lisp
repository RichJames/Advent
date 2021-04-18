;;;;
;;;;  Advent of Code 2016
;;;;     Day 10

(defpackage #:2016Day10
  (:use #:cl))

(in-package #:2016Day10)

(defstruct bot
  (lower   nil)
  (higher  nil)
  (value-1 nil)
  (value-2 nil))

(defparameter *bots* (make-array 200 :element-type 'bot :adjustable t :fill-pointer 0))
(defparameter *outputs* nil)

(defparameter *input-file* "~/quicklisp/local-projects/rich/advent/2016/Day10.txt")
(defparameter *test-file*  "~/quicklisp/local-projects/rich/advent/2016/Day10test.txt")

(defun reset ()
  (setf (fill-pointer *bots*) 0)
  (setf *outputs* nil))

(defun value-instr-p (instr)
  "Returns true if instr is a value instruction, otherwise returns nil."
  (search "value" instr))

(defun give-instr-p (instr)
  "Returns true if instr is a give instruction, otherwise returns nil."
  (search "gives" instr))

(defun parse-give-instr (instr)
  "Decomposes a give instruction, returning the bot that is giving and the type and id of the low and high bots it gives to."
  (let ((give-regex "(\\d+)\\sgives\\slow\\sto\\s(\\w+)\\s(\\d+)\\sand\\shigh\\sto\\s(\\w+)\\s(\\d+)"))
    (ppcre:register-groups-bind ((#'parse-integer bot)
                                 low-target
                                 (#'parse-integer low-id)
                                 high-target
                                 (#'parse-integer high-id))
        (give-regex instr)
      (list bot low-target low-id high-target high-id))))

(defun parse-value-instr (instr)
  "Decomposes a value instruction, returning the value and the bot it is assigned to."
  (let* ((value-regex "^value\\s(\\d+)\\sgoes\\sto\\sbot\\s(\\d+)"))
    (ppcre:register-groups-bind ((#'parse-integer value)
                                 (#'parse-integer bot))
        (value-regex instr)
      (list value bot))))

(defun prepare-bots (file)
  (reset)
  (read-and-record-instrs file))

(defun initialize-outputs (instrs)
  (let* ((output-ids    (mapcan #'(lambda (x) (list (if (string= "output" (nth 1 x))
                                                        (nth 2 x)
                                                        0)
                                                    (if (string= "output" (nth 3 x))
                                                        (nth 4 x)
                                                        0)))
                                instrs))
         (highest-id    (car (reverse (sort output-ids #'<)))))
    (setf *outputs* (make-array (1+ highest-id) :initial-element 0))))

;; Is there a better, cleaner, more idiomatic way to do all of the below?
(defun read-and-record-instrs (file)
  "Reads the instructions from the input file and sets up the bots accordingly in the *bots* array."
  (let ((value-instrs nil)
        (give-instrs  nil))

    (with-open-file (stream file)
      
      (loop :for instr = (read-line stream nil nil)
            :while instr
            :if (give-instr-p instr)
              :collect (parse-give-instr instr) :into give-instructions
            :if (value-instr-p instr)
              :collect (parse-value-instr instr) :into value-instructions
            :finally (progn
                       (setf value-instrs value-instructions)
                       (setf give-instrs (sort give-instructions #'< :key #'car)))))

    (loop :for instr :in give-instrs
          :for (bot-id low-target low-id high-target high-id) = instr
          :do (vector-push-extend (make-bot
                                   :lower (format nil "~a ~a" low-target low-id)
                                   :higher (format nil "~a ~a" high-target high-id))
                                  *bots*))

    (loop :for instr :in value-instrs
          :for (value bot) = instr 
          :do (let ((bot-copy (aref *bots* bot)))
                (if (null (bot-value-1 bot-copy))
                    (setf (bot-value-1 bot-copy) value)
                    (setf (bot-value-2 bot-copy) value))
                (setf (aref *bots* bot) bot-copy)))

    (initialize-outputs give-instrs)))

(defun bot-ready-p (bot)
  "Returns true if bot has both its values; otherwise, returns nil."
  (declare (bot bot))
  (and (bot-value-1 bot) (bot-value-2 bot)))

(defun activate-bot (bot)
  "Applies bot's instructions, passing its values accordingly and setting its own values to nil."
  (let* ((active-bot  (aref *bots* bot))
         (active-val1 (bot-value-1 active-bot))
         (active-val2 (bot-value-2 active-bot))
         (low-val     (if (< active-val1 active-val2) active-val1 active-val2))
         (high-val    (if (= low-val active-val1) active-val2 active-val1)))
    (if (bot-ready-p active-bot)
        (let ((target-regex "(\\w+)\\s(\\d+)"))
          (ppcre:register-groups-bind (target (#'parse-integer target-id)) (target-regex (bot-lower active-bot))
            (if (string= "bot" target)
                (update-bot bot target-id low-val :lower t)
                (update-output bot target-id low-val :lower t)))
          (ppcre:register-groups-bind (target (#'parse-integer target-id)) (target-regex (bot-higher active-bot))
            (if (string= "bot" target)
                (update-bot bot target-id high-val)
                (update-output bot target-id high-val)))))))

(defun update-bot (bot-id target-bot-id value &key (lower nil))
  (let* ((target-bot     (aref *bots* target-bot-id))
         (target-value1  (bot-value-1 target-bot))
         (target-value2  (bot-value-2 target-bot)))
    (if (not (and target-value1 target-value2))
        (progn
          (if target-value1
              (setf (bot-value-2 (aref *bots* target-bot-id)) value)
              (setf (bot-value-1 (aref *bots* target-bot-id)) value))
          (if lower
              (setf (bot-value-1 (aref *bots* bot-id)) nil)
              (setf (bot-value-2 (aref *bots* bot-id)) nil))))))

(defun update-output (bot-id target-id value &key (lower nil))
  (setf (aref *outputs* target-id) value)
  (if lower
      (setf (bot-value-1 (aref *bots* bot-id)) nil)
      (setf (bot-value-2 (aref *bots* bot-id)) nil)))

(defun compares-61-with-17-p (bot)
  (declare (bot bot))
  (and (and (bot-value-1 bot) (bot-value-2 bot))
       (or (and (= (bot-value-1 bot) 61) (= (bot-value-2 bot) 17))
           (and (= (bot-value-1 bot) 17) (= (bot-value-2 bot) 61)))))

(defun activate-bots ()
  (loop :with processed-a-bot = nil
        :for bot :across *bots*
        :for i :upfrom 0
        :if (compares-61-with-17-p bot)
          :collect i :into found-bot
        :if (bot-ready-p bot)
          :do (progn
                (setf processed-a-bot t)
                (activate-bot i))
        :finally (return (list processed-a-bot found-bot))))

(defun part1 ()
  (prepare-bots *input-file*)
  (loop :for (keep-processing found-bot) = (activate-bots)
        :for i :upfrom 1
        :while (and keep-processing (not found-bot))
        :finally (format t "bot that compares 61 & 17: ~a~%" found-bot)))

(defun test ()
  (prepare-bots *test-file*)
  (loop :for (keep-processing found-bot) = (activate-bots)
        :while (and keep-processing (not found-bot))
        :finally (return found-bot)))

(defun any-ready-bots ()
  (loop :for bot :across *bots*
        :for i :upfrom 0
        :if (bot-ready-p bot)
          :collect i))

(defun get-vals (list)
  (loop :for bot-id :in list
        :collect (list bot-id (bot-value-1 (aref *bots* bot-id)) (bot-value-2 (aref *bots* bot-id)))))
