;;;;
;;;;  Advent of Code 2016
;;;;     Day 11

(defpackage #:2016Day11
  (:use #:cl))

(in-package #:2016Day11)

(defclass bldg-floor ()
  ((chips       :initarg :chips      :initform nil :accessor chips)
   (generators  :initarg :generators :initform nil :accessor generators)))

(defclass elevator ()
  ((current-floor :initarg :current-floor :initform 0   :accessor current-floor)
   (chips         :initarg :chips         :initform nil :accessor chips)
   (generators    :initarg :generators    :initform nil :accessor generators)))

(defparameter *facility* (make-array 4 :element-type 'bldg-floor))
(defparameter *elevator* (make-instance 'elevator))
(defparameter *materials* '(promethium cobalt curium ruthenium plutonium))

(defun reset ()
  (setf *elevator* (make-instance 'elevator))
  (setf (aref *facility* 0) (make-instance 'bldg-floor
                                           :chips (list (car *materials*))
                                           :generators (list (car *materials*)))

        (aref *facility* 1) (make-instance 'bldg-floor
                                           :chips nil
                                           :generators (cdr *materials*))
        
        (aref *facility* 2) (make-instance 'bldg-floor
                                           :chips (cdr *materials*)
                                           :generators nil)

        (aref *facility* 3) (make-instance 'bldg-floor
                                           :chips nil
                                           :generators nil)))

(defun change-floor (elevator direction)
  (with-accessors ((e-floor current-floor)) elevator
    (let ((new-floor (cond ((eq direction 'up) (1+ e-floor))
                           ((eq direction 'down) (1- e-floor))
                           (t e-floor))))
      (if (and (>= new-floor 0) (< new-floor (length *facility*)))
          (setf e-floor new-floor)))))

(defun is-safe-p ()
  "Returns t if the state of the facility is safe; otherwise, returns nil."
  (with-accessors ((e-floor current-floor) (e-chips chips) (e-gens generators)) *elevator*
    (loop :with safe = t
          :for floor :across *facility*
          :for i :upfrom 0
          :do (with-accessors ((fl-chips chips) (fl-gens generators)) floor
                (let ((chips (if (= e-floor i )
                                 (union e-chips fl-chips)
                                 fl-chips))
                      (gens  (if (= e-floor i)
                                 (union e-gens fl-gens)
                                 fl-gens)))
                  (if (and (and chips gens)
                           (set-difference chips gens))
                      (setf safe nil))))
          :finally (return safe))))

(defun pick-up (elevator &key (chips nil) (generators nil))
  (with-accessors ((e-floor current-floor) (e-chips chips) (e-gens generators)) elevator
    (with-accessors ((fl-chips chips) (fl-gens generators)) (aref *facility* e-floor)
      (setf e-chips  (union  chips e-chips)
            e-gens   (union  generators e-gens)
            fl-chips (set-exclusive-or chips fl-chips)
            fl-gens  (set-exclusive-or generators fl-gens)))))

(defun drop-off (elevator &key (chips nil) (generators nil))
  (with-accessors ((e-floor current-floor) (e-chips chips) (e-gens generators)) elevator
    (with-accessors ((fl-chips chips) (fl-gens generators)) (aref *facility* e-floor)
        (setf fl-chips (union chips fl-chips)
              fl-gens  (union generators fl-gens)
              e-chips  (set-exclusive-or chips e-chips)
              e-gens   (set-exclusive-or generators e-gens)))))

(defun display-state ()
  (with-accessors ((e-floor current-floor) (e-chips chips) (e-gens generators)) *elevator*
    (format t "Elevator floor: ~a, elevator chips: ~a, elevator generators: ~a~%~%" e-floor e-chips e-gens)

    (loop :for b-floor :across (reverse *facility*)
          :for i :downfrom 3
          :do (with-accessors ((fl-chips chips) (fl-gens generators)) b-floor
                (let ((fl-ind  (if (= e-floor i) "*" " ")))
                  (format t "Floor: ~a,~a floor chips: ~a, floor generators: ~a~%~%" i fl-ind fl-chips fl-gens))))

    (format t "Safe?: ~a~%" (is-safe-p))))



;;; ********* Experiments *********

;;; The below is an interesting effort, but lists won't be efficient if we have to search large lists.  Howver, that
;;; may not be an issue with this problem as the facility is fairly small.  The performance will be with traversing
;;; large numbers of stategies for solving the problem.

(defun create-facility ()
  (copy-tree'(((a) (a))
              (nil (b c d e))
              ((b c d e) nil)
              (() ()))))

(defun create-elevator ()
  (copy-tree '(1 () ())))

(defparameter *facility* (create-facility))

(defparameter *elevator* (create-elevator))

(defparameter *final-state* "1,2,3,4abcde,abcde")

(defun reset ()
  (setf *facility* (create-facility)
        *elevator* (create-elevator))
  (display-state))


(defun get-floor (num)
  (cond ((= num 1) (first *facility*))
        ((= num 2) (second *facility*))
        ((= num 3) (third *facility*))
        ((= num 4) (fourth *facility*))))

;;; Need to modify this function to check if the state after the move is a new state or not.  If
;;; it is a new state, save that state in the hash-table.  If it is not a new state, undo the move.
(defun change-floor (direction)
  (let ((previous-elevator (copy-tree *elevator*))
        (previous-state (get-state)))
    (if (or (second *elevator*) (third *elevator*))
        (progn
          (cond ((and (eq direction 'up) (< (first *elevator*) 4)) (incf (first *elevator*)))
                ((and (eq direction 'down) (> (first *elevator*) 1)) (decf (first *elevator*))))))
    (if (gethash (get-state) *ht*)
        (setf *elevator* previous-elevator)
        (setf (gethash (get-state)) t))))

(defun save-state ()
  (let ((current-state (get-state)))
    (cond ((string= current-state *final-state*) (values t t))
          ((null (gethash current-state *ht*))   (values (setf (gethash current-state *ht*) t) nil))
          (t                                     (values nil nil)))))

(defun display-state ()
  (loop :with e-floor = (first *elevator*)
        :for i :downfrom 4 :to 1
        :for fl = (get-floor i)
        :do (if (= e-floor i)
                (format t "Floor: ~a,* chips: ~a, generators: ~a, **Elevator: chips: ~a, generators: ~a~%"
                        i (first fl) (second fl) (second *elevator*) (third *elevator*))
                (format t "Floor: ~a,  chips: ~a, generators: ~a~%"
                        i (first fl) (second fl))))
  (format t "Safe?: ~a~%" (is-safe-p)))

(defun get-state ()
  "Produce a state string suitable hashing and comparing to other state strings."
  (with-output-to-string (out)
    (loop :with e-floor = (first *elevator*)
          :for i from 1 :to 4
          :for fl = (get-floor i)
          :do (let ((chips (if (= e-floor i)
                               (sort (copy-list (union (first fl) (second *elevator*))) #'string<)
                               (sort (copy-list (first fl)) #'string<)))
                    (gens  (if (= e-floor i)
                               (sort (copy-list (union (second fl) (third *elevator*))) #'string<)
                               (sort (copy-list (second fl)) #'string<))))
                (format out "~a~{~a~},~{~a~}" i chips gens)))
    out))

(defun is-safe-p ()
  "Returns t if the state of the facility is safe; otherwise, returns nil."
  (loop :with safe = t
        :with e-floor = (first *elevator*)
        :for i :from 1 :to 4
        :for fl = (get-floor i)
        :do (let ((chips (if (= e-floor i )
                             (union (second *elevator*) (first fl))
                             (first fl)))
                  (gens  (if (= e-floor i)
                             (union (third *elevator*) (second fl))
                             (second fl))))
              (if (and (and chips gens)
                       (set-difference chips gens))
                  (setf safe nil)))
        :finally (return safe)))

(defun pick-up (&key (chips nil) (generators nil))
  (let ((fl (get-floor (first *elevator*))))
    
    (if (and (<= (+ (length chips) (length (second *elevator*))) 2)
             (<= (+ (length generators) (length (third *elevator*))) 2))

        (progn
          (if (and chips (intersection chips (first fl)))
              (setf (second *elevator*) (union            chips (second *elevator*))
                    (first fl)          (set-exclusive-or chips (first fl))))

          (if (and generators (intersection generators (second fl)))
              (setf (third *elevator*) (union            generators (third *elevator*))
                    (second fl)        (set-exclusive-or generators (second fl))))))))

(defun drop-off (&key (chips nil) (generators nil))
  (let ((fl (get-floor (first *elevator*))))
    
    (if (and chips (not (set-difference chips (second *elevator*))))
        (setf (first fl)          (union            chips (first fl))
              (second *elevator*) (set-exclusive-or chips (second *elevator*))))

    (if (and generators (not (set-difference generators (third *elevator*))))
        (setf (second fl)        (union            generators (second fl))
              (third *elevator*) (set-exclusive-or generators (third *elevator*))))))

(defparameter *ht* (make-hash-table :test 'equal))




;;; Representing the facility state as a bit vector.
;;; Reading left to right, the bits are:
;;;  1-12: the state of the elevator
;;;     - the first 2 bits are the floor number (0-based)
;;;     - the remaining 10 bits are two sets of 5 bits.  The first set is indicates what
;;;       microchips the elevator has; the second set indicates the generators.
;;;       Each set of 5 bits is on/off based on if item a,b,c,d, or e is being carried.
;;;  13-22: 10 bits representing what chips and generators are on floor 1
;;;      - These are two sets of 5 bits, representing chips and generators as is done
;;;        with the elevator
;;;  23-32: 10 bits representing floor 2, similar to floor 1
;;;  33-42: 10 bits representing floor 3, similar to floor 1
;;;  43-52: 10 bits representing floor 4, similar to floor 1

(defparameter *facility* #*0000000000001000010000000000111101111000000000000000)

;;; An alternative might be to just work directly with binary numbers
(defparameter *facility* #b0000000000001000010000000000111101111000000000000000)

;;; This is the end-goal: all chips and microchips on floor 4.  The moment this is achieved,
;;; the elevator will also be on floor 4 (having just brought the final components).

(defparameter *end-goal* #*1100000000000000000000000000000000000000001111111111)

;;; Binary number alternative:
(defparameter *end-goal* #b1100000000000000000000000000000000000000001111111111)


(defun reset ()
  (setf *facility* #b0000000000001000010000000000111101111000000000000000))

;;; Only need this if I am working with bit vectors
(defun bitvec->integer (bitvec)
  (reduce (lambda (a b) (+ (* 2 a) b)) bitvec))

(defmacro e-floor ()
  `(ldb (byte 2 50) *facility*))

(defmacro e-bits ()
  `(ldb (byte 10 40) *facility*))

(defmacro floor-bits (floor-num)
  `(ldb (byte 10 (- 30 (* 10 ,floor-num))) *facility*))

(defmacro chip-bits (bits)
  `(ldb (byte 5 5) ,bits))

(defmacro gen-bits (bits)
  `(ldb (byte 5 0) ,bits))

(defun bits->vals (bits)
  (flet ((get-bit (bit)
           (ldb (byte 1 bit) bits)))
    (list (remove-if #'null (list (if (= (get-bit 9) 1) 'a)
                                  (if (= (get-bit 8) 1) 'b)
                                  (if (= (get-bit 7) 1) 'c)
                                  (if (= (get-bit 6) 1) 'd)
                                  (If (= (Get-bit 5) 1) 'e)))
          (remove-if #'null (list (if (= (get-bit 4) 1) 'a)
                                  (if (= (get-bit 3) 1) 'b)
                                  (if (= (get-bit 2) 1) 'c)
                                  (if (= (get-bit 1) 1) 'd)
                                  (if (= (get-bit 0) 1) 'e))))))


(defun vals->bits (vals)
  )

(defun display-state (&optional (facility *facility*))
  (let ((*facility* facility))
    (loop :with e-floor = (e-floor)
          :with e-vals  = (bits->vals (e-bits))
          :for i :downfrom 3 :to 0
          :for floor-vals = (bits->vals (floor-bits i))
          :do (if (= e-floor i)
                  (format t "Floor: ~a,* chips: ~a, generators: ~a, **Elevator: chips: ~a, generators: ~a~%"
                          (1+ i) (first floor-vals) (second floor-vals) (first e-vals) (second e-vals))
                  (format t "Floor: ~a,  chips: ~a, generators: ~a~%"
                          (1+ i) (first floor-vals) (second floor-vals))))))

(defun change-floor (direction)
  (let ((current-floor (e-floor)))
    (cond ((and (eq direction 'up) (< current-floor 3)) (setf (e-floor) (1+ current-floor)))
          ((and (eq direction 'down) (> current-floor 0)) (setf (e-floor) (1- current-floor))))))

(defun is-safe-p ()
  (loop :with safe = t
        :for i :from 0 :to 3
        :do (let ((chips (if (= (e-floor) i )
                             (logand (chip-bits (e-bits)) (chip-bits (floor-bits i)))
                             (chip-bits (floor-bits i))))
                  (gens  (if (= (e-floor) i)
                             (logand (gen-bits (e-bits)) (gen-bits (floor-bits i)))
                             (gen-bits (floor-bits i)))))
              (if (not (or (= chips 0)
                           (= gens 0)
                           (= chips (logand chips gens))))
                  (setf safe nil)))
        :finally (return safe)))

(defun count-bits (bits &optional (size 5))
  (loop :for i :below size
        :sum (ldb (byte 1 i) bits)))

(defun pick-up (&key (chips 0) (generators 0))
  (let* ((elev-floor    (e-floor))
         (floor-chips   (chip-bits (floor-bits elev-floor)))
         (floor-gens    (gen-bits (floor-bits elev-floor)))
         (new-e-chips   (logior (chip-bits (e-bits)) chips))
         (new-e-gens    (logior (gen-bits (e-bits)) generators))
         (new-fl-chips  (logxor floor-chips chips))
         (new-fl-gens   (logxor floor-gens generators)))
    
    (if (<= (+ (count-bits new-e-chips) (count-bits new-e-gens)) 2)

        (progn
          (if (= chips (logand chips floor-chips))
              (setf (chip-bits (e-bits)) new-e-chips
                    (chip-bits (floor-bits elev-floor)) new-fl-chips))

          (if (= generators (logand generators floor-gens))
              (setf (gen-bits (e-bits)) new-e-gens
                    (gen-bits (floor-bits elev-floor)) new-fl-gens))))))

(defun drop-off (&key (chips nil) (generators nil))
  )

(defun print-bits (n size)
  (format t "~v,'0b" size (ldb (byte size 0) n)))

(deftest test-pickup ()
  (combine-results
   (test-pickup-chips)
   (test-pickup-gens)
   (test-pickup-chips-and-gens)))

(deftest test-pickup-chips ()
  (check
   (= (test-pick-up) *facility*)
   (= (test-pick-up :chips #b10000) #b0010000000000000010000000000111101111000000000000000)
   (= (test-pick-up :chips #b01000) #b0000000000001000010000000000111101111000000000000000)
   (= (test-pick-up :chips #b00100) #b0000000000001000010000000000111101111000000000000000)
   (= (test-pick-up :chips #b00010) #b0000000000001000010000000000111101111000000000000000)
   (= (test-pick-up :chips #b00001) #b0000000000001000010000000000111101111000000000000000)
   (= (test-pick-up :chips #b01000) #b0000000000001000010000000000111101111000000000000000)))

(deftest test-pickup-gens ()
  (check
   (= (test-pick-up :generators #b10000) #b0000000100001000000000000000111101111000000000000000)
   (= (test-pick-up :generators #b01000) #b0000000000001000010000000000111101111000000000000000)
   (= (test-pick-up :generators #b00100) #b0000000000001000010000000000111101111000000000000000)
   (= (test-pick-up :generators #b00010) #b0000000000001000010000000000111101111000000000000000)
   (= (test-pick-up :generators #b00001) #b0000000000001000010000000000111101111000000000000000)))

(defun test-pick-up (&key (chips 0) (generators 0) (init-state state))
  (let ((*facility* state))
    (pick-up :chips chips :generators generators)
    *facility*))
