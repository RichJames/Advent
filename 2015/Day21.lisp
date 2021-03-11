;;;;
;;;; Advent of Code 2015
;;;;   Day 21

(defpackage #:Day21
  (:use #:cl))

(in-package #:Day21)

(defclass player ()
  ((hit-points  :initarg :hit-points :accessor hit-points)
   (damage      :initarg :damage     :accessor damage)
   (armor       :initarg :armor      :accessor armor)
   (spend       :initarg :spend      :accessor spend)))

(defparameter *boss* (make-instance 'player
                                    :hit-points 104 
                                    :damage       8
                                    :armor        1
                                    :spend        0))

(defparameter *me* (make-instance 'player
                                  :hit-points 100 
                                  :damage       0
                                  :armor        0
                                  :spend        0))

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
                        (platemail  102 0 5)))

(defparameter *rings* '((damage+1   25 1 0)
                        (damage+2   50 2 0)
                        (damage+3  100 3 0)
                        (defense+1  20 0 1)
                        (defense+2  40 0 2)
                        (defense+3  80 0 3)))

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

;; With no armor, after 13 rounds I will lose.  The boss does 8 damage and I have 100 hit points.
;; I need to inflict at least 8 damage each round to win.  That means I need weapons that inflict at
;; least 9 damage (the boss has 1 armor).

;; I need to find the cheapest solution where rounds to kill me >= rounds to kill the boss.
;; rounds to kill me   = (/ my-hit-points (- boss-damage my-armor))
;; rounds to kill boss = (/ boss-hit-points (- my-damage boss-armor))
;;  I.E.
;; rounds to kill me   = (/ 100 (- 8 my-armor))
;; rounds to kill boss = (/ 104 (- my-damage 1))
;;
;; So, (/ 100 (- 8 my-armor) >= (/ 104 (- my-damage 1))
;; Reduced, this is 25*my-damage + 26*my-armor >= 233
;;
;; Expressing my-damage and my-armor in terms of each other, we get:
;; my-armor  >= (/ (- 233 (* 25 my-damage)) 26)
;; my-damage >= (/ (- 233 (* 26 my-armor)) 25)

;; I have to make a list of all the possible equipment combinations I am allowed to buy, then assess which of
;; these would guarantee a win.  Of those, I need to pick the cheapest option.

(defparameter *weapons-list* '(dagger shortsword warhammer longsword greataxe))
(defparameter *armor-list* '(leather chainmail splintmail bandedmail platemail))
(defparameter *rings-list* '(damage+1 damage+2 damage+3 defense+1 defense+2 defense+3))

(defparameter *combinations* nil)

(defun build-part1-combos ()
  (build-item-combos *weapons-list* *armor-list* *rings-list*))

(defun build-item-combos (weapons-list armor-list rings-list)
  (append (build-only-weapons-combos weapons-list)
          (build-weapon-and-armor-combos weapons-list armor-list)
          (build-weapon-and-ring-combos weapons-list rings-list)
          (build-weapon-armor-and-ring-combos weapons-list armor-list rings-list)))

(defun validate-item-combos (combos)
  (loop :for combo :in combos
        :if (not (valid-item-combo-p combo))
          :return combo
        :finally (return t)))

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

(defun compute-winner-costs (combos)
  (loop :for combo :in combos
        :collect (list (combo-cost combo) combo) :into combo-costs
        :finally (return (sort combo-costs #'< :key 'car))))

(defun combo-cost (combo)
  (let* ((weapon            (intersection combo *weapons-list*))
         (weapon-cost       (nth 1 (find (car weapon) *weapons* :key 'car)))
         (armor             (intersection combo *armor-list*))
         (armor-cost        (if armor (nth 1 (find (car armor) *armor* :key 'car)) 0))
         (rings             (intersection combo *rings-list*)))
    (loop :for ring :in rings
          :sum (nth 1 (find ring *rings* :key 'car)) :into rings-cost
          :finally (return (+ weapon-cost armor-cost rings-cost)))))

(defun winning-combo-p (combo)
  (let* ((my-attack           (my-damage combo))         
         (my-armor            (my-defense combo))
         (my-hit-points       (hit-points *me*))
         (my-net-damage       (max 1 (- my-attack (armor *boss*))))
         (boss-net-damage     (max 1 (- (damage *boss*) my-armor)))
         (rounds-to-kill-me   (/ my-hit-points boss-net-damage))
         (rounds-to-kill-boss (/ (hit-points *boss*) my-net-damage)))
    (<= rounds-to-kill-boss rounds-to-kill-me)))

(defun my-defense (combo)
  (let* ((armor         (intersection combo *armor-list*))
         (rings         (intersection combo *rings-list*))
         (armor-defense (if armor (nth 3 (find (car armor) *armor* :key 'car)) 0)))
    (loop :for ring :in rings
          :sum (nth 3 (find ring *rings* :key 'car)) :into ring-defense
          :finally (return (+ armor-defense ring-defense)))))

(defun my-damage (combo)
  (let* ((weapon        (intersection combo *weapons-list*))
         (weapon-damage (nth 2 (find (car weapon) *weapons* :key 'car)))
         (rings         (intersection combo *rings-list*)))
    (loop :for ring :in rings
          :sum (nth 2 (find ring *rings* :key 'car)) :into ring-damage
          :finally (return (+ weapon-damage ring-damage)))))

(defun build-only-weapons-combos (weapons-list)
  (loop :for weapon :in weapons-list
        :collect (list weapon)))

(defun build-weapon-and-armor-combos (weapons-list armor-list)
  (loop :with combos = nil
        :for weapon :in weapons-list
        :do (setf combos (append combos (loop :for armor :in armor-list
                                              :collect (list weapon armor))))
        :finally (return combos)))

(defun build-weapon-and-ring-combos (weapons-list rings-list)
  (loop :with combos = nil
        :with ring-combos = (build-ring-combos rings-list)
        :for weapon :in weapons-list
        :do (setf combos (append combos (loop :for ring-list :in ring-combos
                                              :collect (append (list weapon) ring-list))))
        :finally (return combos)))

(defun build-weapon-armor-and-ring-combos (weapon-list armor-list rings-list)
  (loop :with combos = nil
        :with weapon-ring-combos = (build-weapon-and-ring-combos weapon-list rings-list)
        :for armor :in armor-list
        :do (setf combos (append combos (loop :for weapon-ring-combo in weapon-ring-combos
                                              :collect (append (list armor) weapon-ring-combo))))
        :finally (return combos)))

(defun build-ring-combos (rings-list)
  (append (build-single-ring-combos rings-list)
          (build-two-ring-combos rings-list)))

(defun build-single-ring-combos (rings-list)
  (loop :for ring :in rings-list
        :collect (list ring)))

(defun build-two-ring-combos (rings-list)
  (loop :with combos = nil
        :with ring2-list = rings-list
        :for ring1 :in rings-list
        :do (progn
              (setf ring2-list (remove ring1 ring2-list))
              (setf combos (append combos (loop :for ring2 :in ring2-list
                                               :collect (list ring1 ring2)))))
        :finally (return combos)))


;; This is how to turn two lists into a list of combinations, e.g. given
;; '(a b c) and '(1 2 3), produce '((a 1) (a 2) (a 3) (b 1) (b 2) (b 3) (c 1) (c 2) (c 3)).
(loop :with combos = nil
      :for i :in '(a b c)
      :do (setf combos (append combos (loop :for j :in '(1 2 3)
                                            :collect (list i j))))
      :finally (return combos))
