;;;;
;;;; Advent of Code 2015
;;;;   Day 11

(defpackage #:Day11
  (:use #:cl))

(in-package #:Day11)

;; Password rules:
;; - must be exactly 8 characters
;; - must be lowercase letters
;; - at least one straight of 3 or more characters, like 'abc', 'defg', up to 'xyz' (no skipping chars)
;; - may not contain 'i', 'o', or 'l'
;; - must contain at least two doubled pairs, like 'aa', 'bb', 'zz', etc.
;; - new passwords 
