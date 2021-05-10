;;;;
;;;;  Advent of Code 2016
;;;;     Day 11

(defpackage #:2016Day11
  (:use #:cl))

(in-package #:2016Day11)

;;; Representing the facility state as a binary number.
;;; Reading left to right, the bits are:
;;;  1-2:  the first 2 bits are the floor number (0-based)
;;;  3-12: 10 bits representing what chips and generators are on floor 1
;;;      - These are two sets of 5 bits, representing chips and generators,
;;;        each bit represents a single chip/generator, bits 11111 = abcde.
;;;  13-22: 10 bits representing floor 2, similar to floor 1
;;;  23-32: 10 bits representing floor 3, similar to floor 1
;;;  33-42: 10 bits representing floor 4, similar to floor 1

(defparameter *part1-facility* #b001000010000000000111101111000000000000000)
(defparameter *test-facility*  #b001100000000000001000000000010000000000000)
(defparameter *facility* *part1-facility*)
(defparameter *end-goal* #b0000000000000000000000000000001111111111) ;; floor position is not included

(defmacro e-floor (facility)
  `(ldb (byte 2 40) ,facility))

(defmacro floor-bits (floor-num facility)
  `(ldb (byte 10 (- 30 (* 10 ,floor-num))) ,facility))

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


(defun display-state (facility)
  (loop :with e-floor = (e-floor facility)
        :for i :downfrom 3 :to 0
        :for floor-vals = (bits->vals (floor-bits i facility))
        :do (format t "Floor: ~a,~a chips: ~a, generators: ~a~%"
                    (1+ i) (if (= e-floor i) "*" " ") (first floor-vals) (second floor-vals))))

(defun change-floor (direction facility)
  (let ((current-floor (e-floor facility)))
    (cond ((and (eq direction 'up) (< current-floor 3)) (setf (e-floor facility) (1+ current-floor)))
          ((and (eq direction 'down) (> current-floor 0)) (setf (e-floor facility) (1- current-floor))))
    facility))

(defun is-safe-p (facility)
  (loop :with safe = t
        :for i :from 0 :to 3
        :do (let ((chips (chip-bits (floor-bits i facility)))
                  (gens  (gen-bits (floor-bits i facility))))
              (if (not (or (= chips 0)
                           (= gens 0)
                           (= chips (logand chips gens))))
                  (setf safe nil)))
        :finally (return safe)))

(defun print-bits (n size)
  (format t "~v,'0b~%" size (ldb (byte size 0) n)))


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
  ((distance :initarg :distance :initform 0)
   (prev-node :initarg :prev-node :initform nil)))

(defparameter *nodes* (make-hash-table))

(defun reset (facility)
  (setf *facility* facility)
  (clrhash *nodes*)
  (setf (gethash facility *nodes*) (make-instance 'node-info))
  (setf *queue* (make-instance 'queue))
  (enqueue facility *queue*))


;; NOTE: This function blindly accepts whatever is passed for chips and generators.  It relies
;; on the calling function to make valid requests.  My get-next-moves function will ensure
;; this is the case.

(defun move (direction &key (facility *facility*) (chips 0) (generators 0))
  (let* ((prev-state    facility)
         (new-state     (change-floor direction facility))
         (prev-floor    (e-floor prev-state))
         (new-floor     (e-floor new-state)))

    (if (/= new-floor prev-floor)
        (let* ((prev-chips   (chip-bits (floor-bits prev-floor new-state)))
               (prev-gens    (gen-bits (floor-bits prev-floor new-state)))
               (new-chips    (chip-bits (floor-bits new-floor new-state)))
               (new-gens     (gen-bits (floor-bits new-floor new-state))))

          (setf (chip-bits (floor-bits new-floor new-state)) (logior new-chips chips)
                (gen-bits (floor-bits new-floor new-state)) (logior new-gens generators)
                (chip-bits (floor-bits prev-floor new-state)) (logxor prev-chips chips)
                (gen-bits (floor-bits prev-floor new-state)) (logxor prev-gens generators))

          (cond ((not (is-safe-p new-state)) (setf new-state prev-state))
                ((gethash new-state *nodes*) (setf new-state prev-state))
                (t (let* ((prev-info (gethash prev-state *nodes*))
                          (prev-dist (slot-value prev-info 'distance)))
                     (enqueue new-state *queue*)
                     (setf (gethash new-state *nodes*) (make-instance 'node-info
                                                                      :distance (1+ prev-dist)
                                                                      :prev-node prev-state)))))))

    new-state))

;;; Move choices rules:
;;; 1. If moving a chip and a generator, only move matching chip and generator
;;; 2. If multiple generators are present, you can only move a generator if its matching
;;;    chip is not present or is being moved with the generator
;;; 3. Chips can always be moved

(defun get-next-moves (facility)
  (let* ((facility-floor      (e-floor facility))
         (next-moves          nil)
         (floor-chips         (chip-bits (floor-bits facility-floor facility)))
         (floor-gens          (gen-bits (floor-bits facility-floor facility)))
         (num-gens            (logcount floor-gens))
         (matching-chips-gens (logand floor-chips floor-gens))
         (masks               '(1 2 4 8 16)))

    ;; matching chips and gens
    (if (/= 0 matching-chips-gens)
        (loop :for mask :in masks
              :if (logtest mask matching-chips-gens)
                :collect (list mask mask) :into moves-pairs
              :finally (setf next-moves (append next-moves moves-pairs))))

    ;; single chips
    (loop :for mask :in masks
          :if (logtest mask floor-chips)
            :collect (list mask 0) :into moves-chips
          :finally (setf next-moves (append next-moves moves-chips)))

    ;; single gens
    (loop :for mask :in masks
          :when (and (= num-gens 1) (logtest mask floor-gens))
            :collect (list 0 mask) :into moves-gens
          :when (and (> num-gens 1) (logtest mask floor-gens) (not (logtest mask floor-chips)))
            :collect (list 0 mask) :into moves-gens
          :finally (setf next-moves (append next-moves moves-gens)))

    ;; find pairs
    (flet ((find-pairs (bits &key (chips t))
             (loop :for mask :in '(3 5 6 9 10 12 17 18 20 24)
                   :if (= (logand bits mask) mask)
                     :collect (if chips (list mask 0) (list 0 mask)) :into moves-pairs
                   :finally (setf next-moves (append next-moves moves-pairs)))))

      (find-pairs floor-chips)
      (find-pairs floor-gens :chips nil))    
    
    next-moves))

(defun end-state-p (facility)
  (let ((facility-state (ldb (byte 40 0) facility)))
    (= facility-state *end-goal*)))

(defun process-queue ()
  (flet ((report-end (node)
           (format t "End state found! Facility: ~a, steps: ~a~%"
                   node
                   (slot-value (gethash node *nodes*) 'distance))))
    
    (loop :named find-end
          :for node = (dequeue *queue*)
          :while node
          :for next-moves = (get-next-moves node)
          :do (loop :for next-move :in next-moves
                    :for (chips generators) = next-move
                    :do (let ((move-up-state   (move 'up :facility node :chips chips :generators generators))
                              (move-down-state (move 'down :facility node :chips chips :generators generators)))
                          (if (end-state-p move-up-state)
                              (return-from find-end (report-end move-up-state)))
                          (if (end-state-p move-down-state)
                              (return-from find-end (report-end move-down-state))))))))

(defun test-part1 ()
  (reset *test-facility*)
  (setf *end-goal* #b0000000000000000000000000000001100011000)  
  (process-queue))

(defun part1 ()
  (reset *part1-facility*)
  (setf *end-goal* #b0000000000000000000000000000001111111111)
  (process-queue))
