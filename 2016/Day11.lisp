;;;;
;;;;  Advent of Code 2016
;;;;     Day 11

(defpackage #:2016Day11
  (:use #:cl))

(in-package #:2016Day11)

;;; Representing the facility state as a binary number.
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

(defparameter *facility* #b0000000000001000010000000000111101111000000000000000)
(defparameter *facility* #b001000010000000000111101111000000000000000)

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
  (list (remove-if #'null (list (if (logbitp 9 bits) 'a)
                                (if (logbitp 8 bits) 'b)
                                (if (logbitp 7 bits) 'c)
                                (if (logbitp 6 bits) 'd)
                                (If (logbitp 5 bits) 'e)))
        (remove-if #'null (list (if (logbitp 4 bits) 'a)
                                (if (logbitp 3 bits) 'b)
                                (if (logbitp 2 bits) 'c)
                                (if (logbitp 1 bits) 'd)
                                (if (logbitp 0 bits) 'e)))))


(defun vals->bits (vals)
  (loop :with bits = 0
        :for val :in vals
        :do (cond ((eq val 'a) (setf bits (logior bits #b10000)))
                  ((eq val 'b) (setf bits (logior bits #b01000)))
                  ((eq val 'c) (setf bits (logior bits #b00100)))
                  ((eq val 'd) (setf bits (logior bits #b00010)))
                  ((eq val 'e) (setf bits (logior bits #b00001))))
        :finally (return bits))  )

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

(defun change-floor (direction &optional (facility *facility*))
  (let ((*facility* facility)
        (current-floor (e-floor)))
    (cond ((and (eq direction 'up) (< current-floor 3)) (setf (e-floor) (1+ current-floor)))
          ((and (eq direction 'down) (> current-floor 0)) (setf (e-floor) (1- current-floor))))
    *facility*))

(defun is-safe-p (&optional (facility *facility*))
  (let ((*facility* facility))
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
          :finally (return safe))))

(defun pick-up (&key (facility *facility*) (chips 0) (generators 0))
  (let* ((*facility*    facility)
         (floor-chips   (chip-bits (floor-bits (e-floor))))
         (floor-gens    (gen-bits (floor-bits (e-floor))))
         (new-e-chips   (logior (chip-bits (e-bits)) chips))
         (new-e-gens    (logior (gen-bits (e-bits)) generators))
         (new-fl-chips  (logxor floor-chips chips))
         (new-fl-gens   (logxor floor-gens generators)))

    (if (<= (+ (logcount new-e-chips) (logcount new-e-gens)) 2)

        (progn
          (if (and (/= chips 0) (= chips (logand chips floor-chips)))
              (setf (chip-bits (e-bits)) new-e-chips
                    (chip-bits (floor-bits (e-floor))) new-fl-chips))

          (if (and (/= generators 0) (= generators (logand generators floor-gens)))
              (setf (gen-bits (e-bits)) new-e-gens
                    (gen-bits (floor-bits (e-floor))) new-fl-gens))))

    *facility*))

(defun drop-off (&key (facility *facility*) (chips 0) (generators 0))
  (let* ((*facility*   facility)
         (e-chips      (chip-bits (e-bits)))
         (e-gens       (gen-bits (e-bits)))
         (new-e-chips  (logxor e-chips chips))
         (new-e-gens   (logxor e-gens generators))
         (new-fl-chips (logior (chip-bits (floor-bits (e-floor))) chips))
         (new-fl-gens  (logior (gen-bits (floor-bits (e-floor))) generators)))

    (if (and (/= chips 0) (= chips (logand chips e-chips)))
        (setf (chip-bits (floor-bits (e-floor))) new-fl-chips
              (chip-bits (e-bits)) new-e-chips))

    (if (and (/= generators 0) (= generators (logand generators e-gens)))
        (setf (gen-bits (floor-bits (e-floor))) new-fl-gens
              (gen-bits (e-bits)) new-e-gens))

    *facility*))

(defun print-bits (n size)
  (format t "~v,'0b" size (ldb (byte size 0) n)))





;;; ***** Breadth-first search *****

;;; To do a breadth-first search, I need to store two values with each "node" or, in this case state, that I examine.
;;; These are: the distance from the original state, which can be computed by adding 1 to the distance its neighbor with
;;; the shortest distance has, and it needs to store the ID of that neighbor.
;;;
;;; For this problem, each state of the factory is a node. The state itself (aka the value of *factory* at that point in
;;; time) is the id of that state.  I can store these in a hash table.  The value I need to store with the corresponding
;;; hash is a struct containing the two values mentioned above.
;;;
;;; To find possible neighbors, I need to produce a set of possible moves I can do from a given state.  Then, each of those
;;; states needs to be tested to see if it results in either the end state we are seeking or a new, safe state that we haven't
;;; seen before.  Any move that results in a previously seen state or in an unsafe state needs to be rejected.
;;;
;;; If we find the end state, we are done: we have found how to get to the end state in the fewest moves.
;;;
;;; If we find a new, safe state, we need to enqueue it and also record it in the hash table, using the prior state as
;;; the previous node value and adding 1 to that node's distance to be the new node's distance.
;;;
;;; The process of finding new neighbors (mentioned above) is driven by emptying the queue (using a FIFO approach).  So
;;; for each enqueued node, I want to do the process of finding the valid neighbors, recording them and enqueueing them.
;;;
;;; Therefore, to start the process, I should record the initial state in the hash table with a distance of 0 and its
;;; previous node values set to nil.  I should also enqueue this node.  Then, I need to kick-off a process (loop) that
;;; processes each entry in the queue (FIFO) to find neighbors and record and enqueue them until the end state is found.

;;; **** queue implementations from Common Lisp Recipes ****
(defclass queue ()
  ((list :initform nil)
   (tail :initform nil)))

(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))

(defmethod enqueue (new-item (queue queue))
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
      (cond ((null list) (setf list new-tail))
            (t (setf (cdr tail) new-tail)))
      (setf tail new-tail))))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t)
    (with-slots (list tail) queue
      (cond ((cddddr list)
             ;; at least five elements, so print ellipsis
             (format stream "(~{~S ~}... ~S)" (subseq list 0 3) (first tail)))
            ;; otherwise, print whole list
            (t (format stream "~:S" list))))))

(defparameter *queue* (make-instance 'queue))

;;; **** end of queue implementation ****

(defclass node-info ()
  ((distance :initform 0)
   (prev-node :initform nil)))

(defparameter *nodes* (make-hash-table))

(defun reset ()
  (setf *facility* #b0000000000001000010000000000111101111000000000000000)
  (clrhash *nodes*)
  (setf (gethash *facility* *nodes*) (make-instance 'node-info))
  (setf *queue* (make-instance 'queue))
  (enqueue *facility* *queue*))

;; This move function is mostly correct, but I need to review how I am using the global *facility* in this code.  I need to
;; ensure that *facility* really does contain the true previous state for this given call.
(defun move (direction &key (facility *facility*) (chips 0) (generators 0))
  (let ((*facility* facility)
        (prev-state *facility*))
    
    (pick-up :chips chips :generators generators)
    (change-floor direction)
    (drop-off :chips chips :generators generators)

    (if (or (not (is-safe-p))
            (gethash *facility* *nodes*))
        (setf *facility* prev-state))

    (if (not (gethash *facility* *nodes*))
        (let* ((prev-info (gethash prev-state *nodes*))
               (prev-dist (slot-value prev-info 'distance)))
          (enqueue *facility* *queue*)
          (setf (gethash *facility* *nodes*) (make-instance 'node-info
                                                            :distance (1+ prev-dist)
                                                            :prev-node prev-state))))

    *facility*))


;;; Move choices rules:
;;; 1. If moving a chip and a generator, only move matching chip and generator
;;; 2. If multiple generators are present, you can only move a generator if its matching
;;;    chip is not present or is being moved with the generator
;;; 3. Chips can always be moved

(defun get-next-moves (&optional (facility *facility*))
  (let* ((*facility*          facility)
         (next-moves          nil)
         (floor-chips         (chip-bits (floor-bits (e-floor))))
         (floor-gens          (gen-bits (floor-bits (e-floor))))
         (num-gens            (logcount floor-gens))
         (matching-chips-gens (logand floor-chips floor-gens))
         (masks               '(1 2 4 8 16)))

    (if (/= 0 matching-chips-gens)
        (loop :for mask :in masks
              :if (logtest mask matching-chips-gens)
                :collect (list mask mask) :into moves-pairs
              :finally (setf next-moves (append next-moves moves-pairs))))

    (loop :for mask :in masks
          :if (logtest mask floor-chips)
            :collect (list mask 0) :into moves-chips
          :finally (setf next-moves (append next-moves moves-chips)))

    (loop :for mask :in masks
          :when (and (= num-gens 1) (logtest mask floor-gens))
            :collect (list 0 mask) :into moves-gens
          :when (and (> num-gens 1) (logtest mask floor-gens) (not (logtest mask floor-chips)))
            :collect (list 0 mask) :into moves-gens
          :finally (setf next-moves (append next-moves moves-gens)))

    next-moves))

(defun end-state-p (&optional facility)
  (let ((*facility* facility)
        (end-goal   #b1100000000000000000000000000000000000000001111111111))
    (= *facility* end-goal)))

;;; Needs much testing!
;;; I also think I could eliminate the elevator chips and generator bits.  Consider reworking this to do that.
;;; The elevator really just servers as an index to what is the current floor.  This controls what can move.
;;; The capacity of the elevator is really just movement rules, so I don't need to literally put things on
;;; an "elevator" in order to move them to a different floor.

(defun process-queue ()
  (flet ((report-end (node)
           (format t "End state found! Facility: ~a, steps: ~a~%"
                                    (print-bits node 52)
                                    (slot-value (gethash node *nodes*) 'distance))))
    (loop :named find-end
          :for node = (dequeue *queue*)
          :while node
          :for next-moves = (get-next-moves node)
          :do (loop :for next-move :in next-moves
                    :for (chips generators) :in next-move
                    :if (end-state-p (move 'up :facility node :chips chips :generators generators))
                      :do (return-from find-end (report-end node))
                    :if (end-state-p (move 'down :facility node :chips chips :generators generators))
                      :do (return-from find-end (report-end node))))))

(defun part1 ()
  (reset)
  (process-queue))
