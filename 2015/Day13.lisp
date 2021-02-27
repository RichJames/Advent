;;;;
;;;; Advent of Code 2015
;;;;   Day 13

(defpackage #:Day13
  (:use #:cl))

(in-package #:Day13)

(defparameter *guests* '(Alice Bob Carol David Frank Eric George Mallory))

(defparameter *guest-relations-list*
  '((Alice   ((Bob   .  54) (Carol  . -81) (David   . -42) (Eric  .  89)
              (Frank . -89) (George .  97) (Mallory . -94)))
    (Bob     ((Alice .   3) (Carol  . -70) (David   . -31) (Eric  .  72)
              (Frank . -25) (George . -95) (Mallory .  11)))
    (Carol   ((Alice . -83) (Bob    .   8) (David   .  35) (Eric  .  10)
              (Frank .  61) (George .  10) (Mallory .  29)))
    (David   ((Alice .  67) (Bob    .  25) (Carol   .  48) (Eric  . -65)
              (Frank .   8) (George .  84) (Mallory .   9)))
    (Eric    ((Alice . -51) (Bob    . -39) (Carol   .  84) (David . -98)
              (Frank . -20) (George .  -6) (Mallory .  60)))
    (Frank   ((Alice .  51) (Bob    .  79) (Carol   .  88) (David .  33)
              (Eric  .  43) (George .  77) (Mallory .  -3)))
    (George  ((Alice . -14) (Bob    . -12) (Carol   .  52) (David .  14)
              (Eric  . -62) (Frank  . -18) (Mallory . -17)))
    (Mallory ((Alice . -36) (Bob    .  76) (Carol   . -34) (David .  37)
              (Eric  .  40) (Frank  .  18) (George  .   7)))))

(defparameter *guest-relations* (make-hash-table :test 'equal))

;; Populate the hash table with guest relations.  Provides easy lookup by
;; guest name.
(defun build-guest-relations ()
  (clrhash *guest-relations*)
  (loop :for entry :in *guest-relations-list*
        :do (setf (gethash (first entry) *guest-relations*) (cadr entry))))

(defstruct seat
  guest
  left-relation
  right-relation)

(defparameter *seats* (make-array (length *guests*) :element-type 'seat :fill-pointer 0))

;; Helper function to retreive the relation information a guest has toward
;; another guest.
(defun get-happiness (guest other)
  (let ((others (gethash guest *guest-relations*)))
    (cdr (assoc other others))))

;; Sum up the total happiness, given the seating arrangement.
(defun seating-happiness ()
  (loop :for s :across *seats*
        :sum (+ (seat-left-relation s) (seat-right-relation s))))

;; Helper function to create a single seat object and assign a guest
;; to it, plus the happiness they have toward their left and right
;; guests.
(defun assign-guest-seat (guest left-guest right-guest)
  (let ((s (make-seat :guest guest
                      :left-relation (get-happiness guest left-guest)
                      :right-relation (get-happiness guest right-guest))))
    (vector-push s *seats*)))

;; Seat all guests, tracking their happiness toward each other
(defun seat-guests (guest-order)
  (setf (fill-pointer *seats*) 0)
  (loop :for guest :in guest-order
        :for i :upto (length guest-order)
        :do (progn
              (cond ((= i 0) (assign-guest-seat guest
                                                (nth (- (length guest-order) 1) guest-order)
                                                (nth (+ i 1) guest-order)))
                    ((= i (- (length guest-order) 1)) (assign-guest-seat guest
                                                                         (nth (- i 1) guest-order)
                                                                         (nth 0 guest-order)))
                    (t (assign-guest-seat guest
                                          (nth (- i 1) guest-order)
                                          (nth (+ i 1) guest-order)))))))

;; Helper function to remove a single item from a list.  Will work even if we had duplicate
;; items (not necessary in this puzzle, though)
(defun reduce-list (i l)
  (remove (nth i l) l :start i :count 1))

;; Create a list of all permutations of items in list l.
(defun permutate (l)
  (cond ((null l) nil)
        ((null (cdr l)) (list l))
        (t (loop :for item :in l
                 :for i :below (length l)
                 :append (mapcar #'(lambda (x) (cons item x)) (permutate (reduce-list i l)))))))

;; Generate all permutations of a given guest list and trial seat each to find the
;; highest happiness.
(defun find-best-order (guest-list)
  (loop :with best-order = guest-list
        :with best-happiness = -99999999
        :for guest-order :in (permutate guest-list)
        :do (progn
              (seat-guests guest-order)
              (let ((happiness (seating-happiness)))
                (if (> happiness best-happiness)
                    (progn
                      (setf best-order guest-order)
                      (setf best-happiness happiness)))))
        :finally (return (values best-order best-happiness))))

;; Part 2

;; I need to add myself to the guest list.  The happiness of anyone sitting next to me is 0,
;; as is my happiness sitting next to any of them.  So what is the best happiness in this
;; situation?

(defparameter *guests* '(Alice Bob Carol David Frank Eric George Mallory Me))

(defparameter *guest-relations-list*
  '((Alice   ((Bob   .  54) (Carol  . -81) (David   . -42) (Eric  .  89)
              (Frank . -89) (George .  97) (Mallory . -94) (Me    .   0)))
    (Bob     ((Alice .   3) (Carol  . -70) (David   . -31) (Eric  .  72)
              (Frank . -25) (George . -95) (Mallory .  11) (Me    .   0)))
    (Carol   ((Alice . -83) (Bob    .   8) (David   .  35) (Eric  .  10)
              (Frank .  61) (George .  10) (Mallory .  29) (Me    .   0)))
    (David   ((Alice .  67) (Bob    .  25) (Carol   .  48) (Eric  . -65)
              (Frank .   8) (George .  84) (Mallory .   9) (Me    .   0)))
    (Eric    ((Alice . -51) (Bob    . -39) (Carol   .  84) (David . -98)
              (Frank . -20) (George .  -6) (Mallory .  60) (Me    .   0)))
    (Frank   ((Alice .  51) (Bob    .  79) (Carol   .  88) (David .  33)
              (Eric  .  43) (George .  77) (Mallory .  -3) (Me    .   0)))
    (George  ((Alice . -14) (Bob    . -12) (Carol   .  52) (David .  14)
              (Eric  . -62) (Frank  . -18) (Mallory . -17) (Me    .   0)))
    (Mallory ((Alice . -36) (Bob    .  76) (Carol   . -34) (David .  37)
              (Eric  .  40) (Frank  .  18) (George  .   7) (Me    .   0)))
    (Me      ((Alice .   0) (Bob    .   0) (Carol   .   0) (David .   0)
              (Eric  .   0) (Frank  .   0) (George  .   0) (Mallory . 0)))))

(defparameter *seats* (make-array (length *guests*) :element-type 'seat :fill-pointer 0))

(defun part2 ()
  (build-guest-relations)
  (find-best-order *guests*))
