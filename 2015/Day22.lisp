;;;;
;;;; Advent of Code 2015
;;;;   Day 22

(defpackage #:Day22
  (:use #:cl))

(in-package #:Day22)

(defstruct spell
  id (cost 0) (damage 0) (healing 0) (armor 0) (mana 0) (duration 0))

(defparameter *magic-missile* (make-spell :id 'm :cost  53 :damage   4))
(defparameter *drain*         (make-spell :id 'd :cost  73 :damage   2 :healing 2))
(defparameter *shield*        (make-spell :id 's :cost 113 :armor    7 :duration 6))
(defparameter *poison*        (make-spell :id 'p :cost 173 :damage   3 :duration 6))
(defparameter *recharge*      (make-spell :id 'r :cost 229 :mana   101 :duration 5))

(defparameter *least-mana* most-positive-fixnum)
(defparameter *winning-spell-sequence* nil)
(defparameter *winning-game* nil)

(defstruct boss
  (hit-points 51) (damage 9))

(defstruct player
  (hit-points 50) (armor 0) (mana 500) (spell-history nil) (total-spend 0))

(defstruct game
  (player (make-player) :type player)
  (boss   (make-boss)   :type boss)
  (shield-timer 0)
  (poison-timer 0)
  (recharge-timer 0))

(defun clone-game (game)
  (let* ((player       (game-player game))
         (boss         (game-boss game))
         (clone-player (make-player :hit-points    (player-hit-points player)
                                    :armor         (player-armor player)
                                    :mana          (player-mana player)
                                    :spell-history (player-spell-history player)
                                    :total-spend   (player-total-spend player)))
         (clone-boss   (make-boss :hit-points (boss-hit-points boss)
                                  :damage     (boss-damage boss))))
    (make-game :player clone-player
               :boss clone-boss
               :shield-timer   (game-shield-timer game)
               :poison-timer   (game-poison-timer game)
               :recharge-timer (game-recharge-timer game))))

(defun game-end-p (game)
  (let ((boss    (game-boss game))
        (player  (game-player game)))
    (cond ((<= (boss-hit-points boss) 0)                          (values t t))
          ((<= (player-hit-points player) 0)                      (values t nil))
          ((<  (player-mana player) (spell-cost *magic-missile*)) (values t nil))
          ((>  (player-total-spend player) *least-mana*)          (values t nil))
          (t                                                      (values nil nil)))))

(defun available-spells (game)
  (if (not (game-end-p game))
      (let* ((avail-spells nil)
             (player       (game-player game))
             (current-mana (player-mana player)))
        (if (>= current-mana (spell-cost *magic-missile*)) (push *magic-missile* avail-spells))
        (if (>= current-mana (spell-cost *drain*))         (push *drain*         avail-spells))
        (if (and (>= current-mana (spell-cost *shield*))
                 (= 0 (game-shield-timer game)))           (push *shield*        avail-spells))
        (if (and (>= current-mana (spell-cost *poison*))
                 (= 0 (game-poison-timer game)))           (push *poison*        avail-spells))
        (if (and (>= current-mana (spell-cost *recharge*))
                 (= 0 (game-recharge-timer game)))         (push *recharge*      avail-spells))
        (reverse avail-spells))))

(defun play-one-turn (game spell)
  ;; player round    
  (setf (player-mana (game-player game))        (- (player-mana (game-player game)) (spell-cost spell))
        (player-total-spend (game-player game)) (+ (player-total-spend (game-player game)) (spell-cost spell)))
  (push (spell-id spell) (player-spell-history (game-player game)))
  (let ((id (spell-id spell)))
    (cond ((eq id 'm) (cast-magic-missle-spell game))
          ((eq id 'd) (cast-drain-spell game))
          ((eq id 's) (cast-shield-spell game))
          ((eq id 'p) (cast-poison-spell game))
          ((eq id 'r) (cast-recharge-spell game))))

  ;; boss round
  (perform-timed-spells game)
  (setf (player-hit-points (game-player game)) (- (player-hit-points (game-player game))
                                                  (max 1 (- (boss-damage (game-boss game)) (player-armor (game-player game)))))))

(defun perform-timed-spells (game)
  (apply-shield-spell game)
  (apply-poison-spell game)
  (apply-recharge-spell game))

(defun apply-shield-spell (game)
  (if (> (game-shield-timer game) 0)
      (progn
        (decf (game-shield-timer game))
        (setf (player-armor (game-player game)) (spell-armor *shield*)))
      (setf (player-armor (game-player game)) 0)))

(defun apply-poison-spell (game)
  (if (> (game-poison-timer game) 0)
      (progn
        (decf (game-poison-timer game))
        (setf (boss-hit-points (game-boss game)) (- (boss-hit-points (game-boss game)) (spell-damage *poison*))))))

(defun apply-recharge-spell (game)
  (if (> (game-recharge-timer game) 0)
      (progn
        (decf (game-recharge-timer game))
        (setf (player-mana (game-player game)) (+ (player-mana (game-player game)) (spell-mana *recharge*))))))

(defun cast-magic-missle-spell (game)
  (setf (boss-hit-points (game-boss game)) (- (boss-hit-points (game-boss game)) (spell-damage *magic-missile*))))

(defun cast-drain-spell (game)
  (setf (boss-hit-points (game-boss game)) (- (boss-hit-points (game-boss game)) (spell-damage *drain*)))
  (setf (player-hit-points (game-player game)) (+ (player-hit-points (game-player game)) (spell-healing *drain*))))

(defun cast-shield-spell (game)
  (setf (game-shield-timer game) (spell-duration *shield*)))

(defun cast-poison-spell (game)
  (setf (game-poison-timer game) (spell-duration *poison*)))

(defun cast-recharge-spell (game)
  (setf (game-recharge-timer game) (spell-duration *recharge*)))

;; To find the least mana that wins, start with the list of available spells and loop over that list
;; For each spell, play a turn with that spell, then get the list of available spells.  For each of those
;; spells, play a turn with that spell, then get the list of available spells, etc.
;; Continue for each recursive sequence until the game ends.  Tally the mana spent along the way and save
;; the best result.

(defun play-game (game)
  (perform-timed-spells game)
  (let ((avail-spells (available-spells game)))
    (loop :for spell :in avail-spells
          :for clone-game = (clone-game game) :then (clone-game game)
          :do (progn
                (play-one-turn clone-game spell)
                (play-game clone-game))
          :finally (multiple-value-bind (game-ended player-won) (game-end-p game)
                     (if (and game-ended
                              player-won
                              (< (player-total-spend (game-player game)) *least-mana*))
                         (progn
                           (setf *least-mana* (player-total-spend (game-player game)))
                           (format t "~a~%" *least-mana*)
                           (setf *winning-spell-sequence* (reverse (player-spell-history (game-player game))))
                           (setf *winning-game* game)))))))

(defun part1 ()
  (setf *least-mana*             most-positive-fixnum
        *winning-spell-sequence* nil
        *winning-game*           nil)
  (let ((first-game (make-game)))
    (play-game first-game))
  *least-mana*)

;; Part 2

;; Game is now in HARD mode, meaning I lose 1 point on each of MY turns. Initially, I understood the directions
;; to say I lose 1 point on each player's turn, meaning both mine and the boss's turns.  Finally, I realized
;; that the intent is to lose 1 point at the start of each turn of Player (aka me, not the boss).

(defun play-game (game)
  (decf (player-hit-points (game-player game))) ; HARD setting effect
  (perform-timed-spells game)
  (let ((avail-spells (available-spells game)))
    (loop :for spell :in avail-spells
          :for clone-game = (clone-game game) :then (clone-game game)
          :do (progn
                (play-one-turn clone-game spell)
                (play-game clone-game))
          :finally (multiple-value-bind (game-ended player-won) (game-end-p game)
                     (if (and game-ended
                              player-won
                              (< (player-total-spend (game-player game)) *least-mana*))
                         (progn
                           (setf *least-mana* (player-total-spend (game-player game)))
                           (format t "~a~%" *least-mana*)
                           (setf *winning-spell-sequence* (reverse (player-spell-history (game-player game))))
                           (setf *winning-game* game)))))))

;; Just call part1 again with the above redefinition of play-game to get the answer.
