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

(defun reset ()
  (setf (fill-pointer *bots*) 0)
  (setf *outputs* nil))


;;; *** Code to read, interpret and save in usable form the input data ***

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

(defun initialize-outputs (instrs)
  "Prepares the *outputs* array, sized based on the input data."
  (let* ((output-ids    (mapcan #'(lambda (x) (list (if (string= "output" (nth 1 x))
                                                        (nth 2 x)
                                                        0)
                                                    (if (string= "output" (nth 3 x))
                                                        (nth 4 x)
                                                        0)))
                                instrs))
         (highest-id    (car (reverse (sort output-ids #'<)))))
    (setf *outputs* (make-array (1+ highest-id) :initial-element 0))))

(defun read-and-record-instrs (file)
  "Reads the instructions from the input file and sets up the bots accordingly in the *bots* array."
  (let ((instructions nil))

    (with-open-file (stream file)
      
      (loop :for instr = (read-line stream nil nil)
            :while instr
            :if (give-instr-p instr)
              :collect (parse-give-instr instr) :into give-instructions
            :if (value-instr-p instr)
              :collect (parse-value-instr instr) :into value-instructions
            :finally (setf instructions (list (sort give-instructions #'< :key #'car) value-instructions))))

    ;; create bot objects in the *bots* array, based on give instructions found in the input file
    (loop :with sorted-instrs = (car instructions)
          :for instr :in sorted-instrs
          :for (bot-id low-target low-id high-target high-id) = instr
          :do (vector-push-extend (make-bot
                                   :lower (format nil "~a ~a" low-target low-id)
                                   :higher (format nil "~a ~a" high-target high-id))
                                  *bots*))

    ;; assign initial values to bots that have them, based on value instructions found in the input file
    (loop :for instr :in (cadr instructions)
          :for (value bot) = instr 
          :do (if (null (bot-value-1 (aref *bots* bot)))
                  (setf (bot-value-1 (aref *bots* bot)) value)
                  (setf (bot-value-2 (aref *bots* bot)) value)))

    ;; create *outputs* array, based on outputs identified in the give instructions
    (initialize-outputs (car instructions))))

(defun prepare-bots (file)
  "Resets and then loads the *bots* and *outputs* arrays, based on instructions in file."
  (reset)
  (read-and-record-instrs file))


;;; *** Code to set the bots in motion, following their instructions ***

(defun bot-ready-p (bot)
  "Returns true if bot has both its values; otherwise, returns nil."
  (declare (bot bot))
  (and (bot-value-1 bot) (bot-value-2 bot)))

(defun activate-bot (bot)
  "Applies bot's instructions, passing its values accordingly and setting its own values to nil."
  (let* ((active-bot  (aref *bots* bot))
         (active-val1 (bot-value-1 active-bot))
         (active-val2 (bot-value-2 active-bot))
         (min-val     (min active-val1 active-val2))
         (max-val     (max active-val1 active-val2)))
    
    (if (bot-ready-p active-bot)
        (let ((target-regex "(\\w+)\\s(\\d+)"))

          (setf (bot-value-1 (aref *bots* bot)) nil)
          (setf (bot-value-2 (aref *bots* bot)) nil)
          
          (ppcre:register-groups-bind (target (#'parse-integer target-id)) (target-regex (bot-lower active-bot))
            (if (string= "bot" target)
                (update-bot target-id min-val)
                (update-output target-id min-val)))
          
          (ppcre:register-groups-bind (target (#'parse-integer target-id)) (target-regex (bot-higher active-bot))
            (if (string= "bot" target)
                (update-bot target-id max-val)
                (update-output target-id max-val)))))))

(defun update-bot (target-bot-id value)
  "Update bot and target-bot, moving value from the former to the latter."
  (let* ((target-bot     (aref *bots* target-bot-id))
         (target-value1  (bot-value-1 target-bot))
         (target-value2  (bot-value-2 target-bot)))
    
    (if (not (and target-value1 target-value2))
        (if target-value1
            (setf (bot-value-2 (aref *bots* target-bot-id)) value)
            (setf (bot-value-1 (aref *bots* target-bot-id)) value)))))

(defun update-output (target-id value)
  "Update bot and the targetted output slot, moving value from the former to the latter."
  (setf (aref *outputs* target-id) value))

(defun has-values-p (bot value1 value2)
  "Returns true if bot has both values; otherwise, returns nil."
  (declare (bot bot))
  (and (and (bot-value-1 bot) (bot-value-2 bot))
       (or (and (= (bot-value-1 bot) value1) (= (bot-value-2 bot) value2))
           (and (= (bot-value-1 bot) value2) (= (bot-value-2 bot) value1)))))

(defun activate-bots ()
  "Loops once through all bots, activating those that have two values and checking which bot has 61 & 17."
  (loop :with processed-a-bot = nil
        :for bot :across *bots*
        :for i :upfrom 0
        :if (has-values-p bot 61 17)
          :collect i :into found-bot
        :if (bot-ready-p bot)
          :do (progn
                (setf processed-a-bot t)
                (activate-bot i))
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
  (* (aref *outputs* 0) (aref *outputs* 1) (aref *outputs* 2)))








