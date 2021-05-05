;;;
;;; Unit test code for Day11 solutions
;;;

(in-package #:2016Day11)

;;; Unit test framework

(load "Day11.lisp")

(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro combine-results (&body forms)
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop :for f :in forms :collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop :for f :in forms :collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defun test-pick-up (&key (chips nil) (generators nil) (init-state *facility*))
  (let ((*facility*  init-state)
        (chip-bits   (if chips (vals->bits chips) 0))
        (gen-bits    (if generators (vals->bits generators) 0)))
    (pick-up :chips chip-bits :generators gen-bits)
    *facility*))

(defun test-drop-off (&key (chips nil) (generators nil) (init-state *facility*))
  (let ((*facility*  init-state)
        (chip-bits   (if chips (vals->bits chips) 0))
        (gen-bits    (if generators (vals->bits generators) 0)))
    (drop-off :chips chip-bits :generators gen-bits)
    *facility*))

(deftest test-pickup-chips ()
  (reset)
  (check
    (= (test-pick-up) *facility*)
    (= (test-pick-up :chips (vals->bits '(a))) #b0010000000000000010000000000111101111000000000000000)
    (= (test-pick-up :chips (vals->bits '(b))) *facility*)
    (= (test-pick-up :chips (vals->bits '(a b))) *facility*)
    (= (test-pick-up :chips (vals->bits '(a b c))) *facility*)
    (change-floor 'up)
    (change-floor 'up)
    (= (test-pick-up :chips (vals->bits '(b c))) #b1001100000001000010000000000111100011000000000000000)
    (= (test-pick-up :chips (vals->bits '(b c d))) #b1000000000001000010000000000111101111000000000000000))
  (reset))

(deftest test-pickup-chips ()
  (reset)
  (check
    (= (test-pick-up) *facility*)
    (= (test-pick-up :chips '(a)) #b0010000000000000010000000000111101111000000000000000)
    (= (test-pick-up :chips '(b)) *facility*)
    (= (test-pick-up :chips '(a b)) *facility*)
    (= (test-pick-up :chips '(a b c)) *facility*)
    (change-floor 'up)
    (change-floor 'up)
    (= (test-pick-up :chips '(b c)) #b1001100000001000010000000000111100011000000000000000)
    (= (test-pick-up :chips '(b c d)) #b1000000000001000010000000000111101111000000000000000))
  (reset))

(deftest test-pickup-gens ()
  (reset)
  (check
    (= (test-pick-up :generators '(a)) #b0000000100001000000000000000111101111000000000000000)
    (= (test-pick-up :generators '(b)) *facility*)
    (= (test-pick-up :generators '(a b)) *facility*)
    (= (test-pick-up :generators '(a b c)) *facility*)
    (change-floor 'up)
    (= (test-pick-up :generators '(b c)) #b0100000011001000010000000000001101111000000000000000)
    (= (test-pick-up :generators '(b c d)) #b0100000000001000010000000000111101111000000000000000)))

(deftest test-pickup-chips-and-gens ()
  (reset)
  (check
    (= (test-pick-up :chips '(a) :generators '(a)) #b0010000100000000000000000000111101111000000000000000)
    (= (test-pick-up :chips '(b) :generators '(a)) #b0000000100001000000000000000111101111000000000000000)
    (= (test-pick-up :chips '(a) :generators '(b)) #b0010000000000000010000000000111101111000000000000000)
    (change-floor 'up)
    (= (test-pick-up :chips '(b) :generators '(b c)) *facility*)
    (change-floor 'up)
    (= (test-pick-up :chips '(b c) :generators '(b)) *facility*)))

(deftest test-pickup ()
  (combine-results
    (test-pickup-chips)
    (test-pickup-gens)
    (test-pickup-chips-and-gens)))

(deftest test-drop-chips ()
  (reset)
  (check
    (= (test-drop-off) *facility*)))

(deftest test-drop-gens ()
  (reset)
  (check
    (= (test-drop-off) *facility*)))

(deftest test-drop-chips-and-gens ()
  (reset)
  (check
    (= (test-drop-off) *facility*)))

(deftest test-drop-off ()
  (combine-results
    (test-drop-chips)
    (test-drop-gens)
    (test-drop-chips-and-gens)))

(deftest test-all ()
  (combine-results
    (test-pick-up)
    (test-drop-off)))
