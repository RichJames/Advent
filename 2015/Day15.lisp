;;;;
;;;; Advent of Code 2015
;;;;   Day 15

(defpackage #:Day15
  (:use #:cl))

(in-package #:Day15)

#|
Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2
Sprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9
Candy: capacity -1, durability 0, flavor 4, texture 0, calories 1
Chocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8
|#

;; Maybe won't use a class for this.
(defclass ingredient ()
  ((name         :initarg :name         :accessor name)
   (capacity     :initarg :capacity     :accessor capacity)
   (durability   :initarg :durability   :accessor durability)
   (flavor       :initarg :flavor       :accessor flavor)
   (texture      :initarg :texture      :accessor texture)
   (calories     :initarg :calories     :accessor calories)))

(defun create-ingredient (name capacity durability flavor texture calories)
  (make-instance 'ingredient
                 :name name
                 :capacity capacity
                 :durability durability
                 :flavor flavor
                 :texture texture
                 :calories calories))

(defparameter *test-ingredient* (create-ingredient "Sugar" 3 0 0 -3 2))

(defun calc-ingredient (ingredient quantity)
  (with-slots (capacity durability flavor texture) ingredient
    (list (* capacity quantity) (* durability quantity) (* flavor quantity) (* texture quantity))))

;; Here's a list approach.  We have so few ingredients, I think I could manage this efficiently
;; enough just using lists.

(defparameter *ingredients* '(("Sugar"      3 0  0 -3 2)
                              ("Sprinkles" -3 3  0  0 9)
                              ("Candy"     -1 0  4  0 1)
                              ("Chocolate"  0 0 -2  2 8)))

(defparameter *test-ingredient* '("Sugar" 3 0 0 -3 2))

(defun calc-ingredient (ingredient quantity)
  (let ((capacity   (second ingredient))
        (durability (third ingredient))
        (flavor     (fourth ingredient))
        (texture    (fifth ingredient)))
    (list (* capacity quantity) (* durability quantity) (* flavor quantity) (* texture quantity))))

(defun calc-total-ingredients-score (sugar sprinkles candy chocolate)
  (let* ((sugar-totals     (calc-ingredient (first  *ingredients*) sugar))
         (sprinkles-totals (calc-ingredient (second *ingredients*) sprinkles))
         (candy-totals     (calc-ingredient (third  *ingredients*) candy))
         (chocolate-totals (calc-ingredient (fourth *ingredients*) chocolate))
         (totals           (mapcar #'+ sugar-totals sprinkles-totals candy-totals chocolate-totals))
         (totals-adj       (mapcar #'(lambda (x) (if (< x 0) 0 x)) totals)))
    (reduce #'* totals-adj)))

;; Now, need a way to generate proportions to test to find the best mix.

(defun find-best-score (total-quantity)
  (let ((best-score 0)
        (best-mix nil))
    (loop :for i :upto total-quantity
          :do (loop :for j :upto (- total-quantity i)
                    :do (loop :for k :upto (- total-quantity i j)
                              :do (loop :for l :upto (- total-quantity i j k)
                                        :do (if (= (+ i j k l) total-quantity)
                                                (let ((score (calc-total-ingredients-score i j k l)))
                                                  (if (> score best-score)
                                                      (progn
                                                        (setf best-score score)
                                                        (setf best-mix (list i j k l)))))))))
          :finally (return (values best-score best-mix)))))

;; Part2 - consider calories

;; The irony of this puzzle is the goal is to find a cookie that is "healthier", but
;; we have to find one that has *exactly* 500 calories.  However, the best cookie from
;; part 1 has less calories than that (462).

(defun calc-ingredient-cals (ingredient quantity)
  (let ((calories (sixth ingredient)))
    (* calories quantity)))

(defun calc-total-calories (sugar sprinkles candy chocolate)
  (let* ((sugar-cals     (calc-ingredient-cals (first  *ingredients*) sugar))
         (sprinkles-cals (calc-ingredient-cals (second *ingredients*) sprinkles))
         (candy-cals     (calc-ingredient-cals (third  *ingredients*) candy))
         (chocolate-cals (calc-ingredient-cals (fourth *ingredients*) chocolate)))
    (+ sugar-cals sprinkles-cals candy-cals chocolate-cals)))

(defun find-best-part2-score (total-quantity)
  (let ((best-score 0)
        (best-mix nil))
    (loop :for i :upto total-quantity
          :do (loop :for j :upto (- total-quantity i)
                    :do (loop :for k :upto (- total-quantity i j)
                              :do (loop :for l :upto (- total-quantity i j k)
                                        :do (if (= (+ i j k l) total-quantity)
                                                (let ((score (calc-total-ingredients-score i j k l))
                                                      (cals  (calc-total-calories i j k l)))
                                                  (if (and (= cals 500) (> score best-score))
                                                      (progn
                                                        (setf best-score score)
                                                        (setf best-mix (list i j k l)))))))))
          :finally (return (values best-score best-mix)))))
