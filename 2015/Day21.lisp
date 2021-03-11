;;;;
;;;; Advent of Code 2015
;;;;   Day 21

(defpackage #:Day21
  (:use #:cl))

(in-package #:Day21)

;; I must buy:
;;  - exactly 1 weapon
;;  - 0 - 1 armor
;;  - 0 - 2 rings
;;
;;  Each item can be purchased only once.
;;  I must used any items I buy (don't know why this is stated; it only drives my costs
;;  up to buy something I won't use).
;;
;;  What is the least amount of gold I can spend and still win the fight?

;; I need to find the cheapest solution where rounds to kill me >= rounds to kill the boss.
;;   rounds to kill me   = (ceiling (/ my-hit-points (- boss-damage my-armor)))
;;   rounds to kill boss = (ceiling (/ boss-hit-points (- my-damage boss-armor)))
;; Note that I have to compute the ceiling of these values.  Rounds are discrete units of time.

;; I have to make a list of all the possible equipment combinations I am allowed to buy, then assess which of
;; these would guarantee a win.  Of those, I need to pick the cheapest option.

;; Attributes are: '(hit-points damage armor)
(defparameter *boss* '(104 8 12))
(defparameter *me*   '(100 0  0))

;; Items are: '(name cost damage armor)
(defparameter *weapons* '((dagger      8 4 0)
                          (shortsword 10 5 0)
                          (warhammer  25 6 0)
                          (longsword  40 7 0)
                          (greataxe   74 8 0)))

(defparameter *armor* '((leather     13 0 1)
                        (chainmail   31 0 2)
                        (splintmail  53 0 3)
                        (bandedmail  75 0 4)
                        (platemail  102 0 5)
                        (none         0 0 0)))

(defparameter *rings* '((damage+1   25 1 0)
                        (damage+2   50 2 0)
                        (damage+3  100 3 0)
                        (defense+1  20 0 1)
                        (defense+2  40 0 2)
                        (defense+3  80 0 3)
                        (none-1      0 0 0)
                        (none-2      0 0 0)))

(defun find-part1-answer ()
  (find-cheapest-winner (find-winning-combos (build-part1-combos))))

(defun build-part1-combos ()
  (build-item-combos *weapons* *armor* *rings*))

(defun build-item-combos (weapons-list armor-list rings-list)
  (loop :with combos = nil
        :for weapon :in weapons-list
        :do (loop :for armor :in armor-list
                  :do (loop :for ring1 :in rings-list
                            :for ring2-list = (remove ring1 rings-list :test #'equal)
                            :do (loop :for ring2 :in ring2-list
                                      :do (setf combos (append combos (list (list (car weapon) (car armor) (car ring1) (car ring2))))))))
        :finally (return combos)))

(defun find-winning-combos (combos)
  (loop :for combo :in combos
        :if (winning-combo-p combo)
          :collect combo :into winners
        :finally (return winners)))

(defun find-cheapest-winner (combos)
  (loop :with best-combo = nil
        :with cheapest-cost = 999
        :for combo :in combos
        :for combo-cost = (combo-cost combo)
        :do (if (< combo-cost cheapest-cost)
                (progn
                  (setf best-combo combo)
                  (setf cheapest-cost combo-cost)))
        :finally (return (values cheapest-cost best-combo))))

(defun combo-cost (combo)
  ;; combo will be a list: '(weapon armor ring1 ring2)
  (let ((weapon-cost   (nth 1 (find (nth 0 combo) *weapons* :key 'car)))
        (armor-cost    (nth 1 (find (nth 1 combo) *armor*   :key 'car)))
        (ring1-cost    (nth 1 (find (nth 2 combo) *rings*   :key 'car)))
        (ring2-cost    (nth 1 (find (nth 3 combo) *rings*   :key 'car))))
    (+ weapon-cost armor-cost ring1-cost ring2-cost)))

(defun winning-combo-p (combo)
  (let* ((my-attack           (my-damage combo))         
         (my-armor            (my-defense combo))
         (my-net-damage       (max 1 (- my-attack (nth 2 *boss*))))
         (boss-net-damage     (max 1 (- (nth 1 *boss*) my-armor)))
         (rounds-to-kill-me   (ceiling (/ (nth 0 *me*)   boss-net-damage)))
         (rounds-to-kill-boss (ceiling (/ (nth 0 *boss*) my-net-damage))))
    (<= rounds-to-kill-boss rounds-to-kill-me)))

(defun my-defense (combo)
  ;; combo will be a list: '(weapon armor ring1 ring2)
  (let ((armor-defense  (nth 3 (find (nth 1 combo) *armor* :key 'car)))
        (ring1-defense  (nth 3 (find (nth 2 combo) *rings* :key 'car)))
        (ring2-defense  (nth 3 (find (nth 3 combo) *rings* :key 'car))))
    (+ armor-defense ring1-defense ring2-defense)))

(defun my-damage (combo)
  ;; combo will be a list: '(weapon armor ring1 ring2)
  (let ((weapon-damage  (nth 2 (find (nth 0 combo) *weapons* :key 'car)))
        (ring1-damage   (nth 2 (find (nth 2 combo) *rings*   :key 'car)))
        (ring2-damage   (nth 2 (find (nth 3 combo) *rings*   :key 'car))))
    (+ weapon-damage ring1-damage ring2-damage)))



;; This is how to turn two lists into a list of combinations, e.g. given
;; '(a b c) and '(1 2 3), produce '((a 1) (a 2) (a 3) (b 1) (b 2) (b 3) (c 1) (c 2) (c 3)).
(loop :with combos = nil
      :for i :in '(a b c)
      :do (setf combos (append combos (loop :for j :in '(1 2 3)
                                            :collect (list i j))))
      :finally (return combos))
