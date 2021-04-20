;;;;
;;;;  Advent of Code 2016
;;;;     Day 10 - alternate solution

(defpackage #:2016Day10alt
  (:use #:cl))

(in-package #:2016Day10alt)

(defparameter *input-file* "~/quicklisp/local-projects/rich/advent/2016/Day10.txt")

;;; *********** class definitions ****************

(defclass bot ()
  ((lower-type  :initarg :lower-type  :initform (error "Must provide lower-type.")  :accessor lower-type)
   (lower-id    :initarg :lower-id    :initform (error "Must provide lower-id.")    :accessor lower-id)
   (higher-type :initarg :higher-type :initform (error "Must provide higher-type.") :accessor higher-type)
   (higher-id   :initarg :higher-id   :initform (error "Must provide higher-id.")   :accessor higher-id)
   (value1      :initarg :value1      :initform nil                                 :accessor value1)
   (value2      :initarg :value2      :initform nil                                 :accessor value2)))

(defclass output ()
  ((value :initarg :value :initform 0 :accessor value)))

;;; *********** generic function definitions ****************

(defgeneric clear-values (bot)
  (:documentation "Sets the value slots in bot to nil."))

(defgeneric is-ready-p (bot)
  (:documentation "Returns true if bot has two non-nil values; otherwise, returns nil."))

(defgeneric add-value (target value)
  (:documentation "Adds the value to target ."))

(defgeneric low-value (bot)
  (:documentation "Returns the low value of bot or, if either of the values is nil, returns nil"))

(defgeneric high-value (bot)
  (:documentation "Returns the high value of bot or, if either of the values is nil, returns nil"))

(defgeneric give-values (bot)
  (:documentation "If bot has two values, gives its values to its assigned targets."))

;;; *********** method definitions ****************

(defmethod clear-values ((bot bot))
  (with-accessors ((val1 value1) (val2 value2)) bot
    (setf val1 nil)
    (setf val2 nil)))

(defmethod is-ready-p ((bot bot))
  (with-accessors ((val1 value1) (val2 value2)) bot
    (and val1 val2)))

(defmethod add-value ((bot bot) value)
  (with-accessors ((val1 value1) (val2 value2)) bot
    (if (null val1)
        (setf val1 value)
        (setf val2 value))))

(defmethod add-value ((output output) value)
  (with-accessors ((val value)) output
    (setf val value)))

(defmethod low-value ((bot bot))
  (with-accessors ((val1 value1) (val2 value2)) bot
    (if (and val1 val2)
        (min val1 val2))))

(defmethod high-value ((bot bot))
  (with-accessors ((val1 value1) (val2 value2)) bot
    (if (and val1 val2)
        (max val1 val2))))

(defmethod give-values ((bot bot))
  (with-accessors ((low-type lower-type) (low-id lower-id) (high-type higher-type) (high-id higher-id)) bot
    (let ((low-target    (get-target low-type low-id))
          (high-target   (get-target high-type high-id)))
      (add-value low-target (low-value bot))
      (add-value high-target (high-value bot))
      (clear-values bot))))

;;; *********** arrays and related helper functions  ****************

(defparameter *bots* (make-array 200 :element-type 'bot :adjustable t :fill-pointer 0))
(defparameter *outputs* (make-array 1 :element-type 'output :adjustable t :fill-pointer 0))

(defun reset ()
  (setf (fill-pointer *bots*) 0)
  (setf (fill-pointer *outputs*) 0))

(defun get-target (target-type target-id)
  (if (string= target-type "bot")
      (aref *bots* target-id)
      (if (string= target-type "output")
          (aref *outputs* target-id))))

;;; *********** Code to read, interpret and save the input data in usable form ****************

(defun value-instr-p (instr)
  "Returns true if instr is a value instruction, otherwise returns nil."
  (search "value" instr))

(defun give-instr-p (instr)
  "Returns true if instr is a give instruction, otherwise returns nil."
  (search "gives" instr))

(defun parse-give-instr (instr)
  "Decomposes a give instruction, returning the bot that is giving and the type and id of the low and high bots it gives to."
  (let ((give-regex "bot (\\d+) gives low to (\\w+) (\\d+) and high to (\\w+) (\\d+)"))
    (ppcre:register-groups-bind ((#'parse-integer bot)
                                 low-target
                                 (#'parse-integer low-id)
                                 high-target
                                 (#'parse-integer high-id))
        (give-regex instr)
      (list bot low-target low-id high-target high-id))))

(defun parse-value-instr (instr)
  "Decomposes a value instruction, returning the value and the bot it is assigned to."
  (let* ((value-regex "^value (\\d+) goes to bot (\\d+)"))
    (ppcre:register-groups-bind ((#'parse-integer value)
                                 (#'parse-integer bot))
        (value-regex instr)
      (list value bot))))

(defun initialize-outputs (instrs)
  "Sets the correct size for the *outputs* array, based on the output targets found in the instructions."
  (let* ((output-ids    (mapcan #'(lambda (x) (list (if (string= "output" (nth 1 x))
                                                        (nth 2 x)
                                                        0)
                                                    (if (string= "output" (nth 3 x))
                                                        (nth 4 x)
                                                        0)))
                                instrs))
         (highest-id    (car (reverse (sort output-ids #'<)))))
    
    (loop :repeat (1+ highest-id)
          :do (vector-push-extend (make-instance 'output) *outputs*))))

(defun read-and-record-instrs (file)
  "Reads the instructions from the input file and sets up the bots accordingly in the *bots* array."
  (let ((instructions nil))

    (with-open-file (stream file)
      
      (loop :for instr = (read-line stream nil nil)
            :while instr
            :if (give-instr-p instr) :collect (parse-give-instr instr) :into give-instructions
            :if (value-instr-p instr) :collect (parse-value-instr instr) :into value-instructions
            :finally (setf instructions (list (sort give-instructions #'< :key #'car) value-instructions))))

    ;; create bot objects in the *bots* array, based on give instructions found in the input file
    (loop :with sorted-instrs = (car instructions)
          :for instr :in sorted-instrs
          :for (bot-id low-target low-id high-target high-id) = instr
          :do (vector-push-extend (make-instance 'bot
                                                 :lower-type  low-target
                                                 :lower-id    low-id
                                                 :higher-type high-target
                                                 :higher-id   high-id)
                                  *bots*))

    ;; assign initial values to bots that have them, based on value instructions found in the input file
    (loop :for instr :in (cadr instructions)
          :for (value bot-id) = instr
          :for bot = (get-target "bot" bot-id)
          :do (add-value bot value))

    ;; create *outputs* array, based on outputs identified in the give instructions
    (initialize-outputs (car instructions))))

(defun prepare-bots (file)
  "Resets and then loads the *bots* and *outputs* arrays, based on instructions in file."
  (reset)
  (read-and-record-instrs file))

;;; *********** Code to set the bots in motion, following their instructions ****************

(defun has-values-p (bot v1 v2)
  "Returns true if bot has both values; otherwise, returns nil."
  (declare (bot bot))
  (if (is-ready-p bot)
      (with-accessors ((val1 value1) (val2 value2)) bot
        (or (and (= val1 v1) (= val2 v2))
            (and (= val1 v2) (= val2 v1))))))

(defun activate-bots ()
  "Loops once through all bots, activating those that have two values and checking which bot has 61 & 17."
  (loop :with processed-a-bot = nil
        :for bot :across *bots*
        :for i :upfrom 0
        :if (has-values-p bot 61 17) :collect i :into found-bot
        :if (is-ready-p bot)
          :do (progn
                (setf processed-a-bot t)
                (give-values bot))
        :finally (return (list processed-a-bot found-bot))))

(defun part1 ()
  "Continuously activate bots until no more can be activated and/or the bot we are looking for is found."
  (prepare-bots *input-file*)
  (loop :for (keep-processing found-bot) = (activate-bots)
        :while (and keep-processing (not found-bot))
        :finally (format t "bot that compares 61 & 17: ~a~%" found-bot)))

;;; ***** Part 2 *****

(defun part2 ()
  (prepare-bots *input-file*)

  (loop :for (keep-processing found-bot) = (activate-bots)
        :while keep-processing)

  (with-accessors ((val0 value)) (aref *outputs* 0)
    (with-accessors ((val1 value)) (aref *outputs* 1)
      (with-accessors ((val2 value)) (aref *outputs* 2)
        (* val0 val1 val2)))))
