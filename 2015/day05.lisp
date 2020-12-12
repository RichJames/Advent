;;; Advent of Code 2015
;;;  Day 5

;;; Part 1

;; Test 1: Test if string contains at least 3 of: aeiou
(defun 3-vowels-p (string)
  (> (length (remove-if-not #'(lambda (c)
				(member c '(#\a #\e #\i #\o #\u)))
			    (string-downcase string)))
     2))

;; Test 2: Test if string contains at least 1 letter that is doubled (e.g. xx, ii, bb)
(defun has-doubles-p (string)
  (loop :with prev-char = nil
	:for char :across string
	:if (equalp char prev-char)
	  :return t
	:else
	  :do (setf prev-char char)))

;; Test 3: Test if string contains any of "ab", "cd", "pq" or "xy"
(defun bad-seq-p (string)
  (> (count-if #'(lambda (s) (search s (string-downcase string))) '("ab" "cd" "pq" "xy"))
     0))

;; Test if string is nice:  Test 1 and Test 2 are true, Test 3 is false.
(defun is-nice-p (string)
  (and (3-vowels-p string)
       (has-doubles-p string)
       (not (bad-seq-p string))))

(defun count-nice-strings (file)
  (with-open-file (myfile file)
    (loop :for line = (read-line myfile nil nil)
	  :while line
	  :counting (is-nice-p line) :into nice-strings-count
	  :finally (return nice-strings-count))))

;;; Part 2

;; Test 1: Contains at least one string pair that appears twice in the string, w/o any
;;         overlapping (e.g. "aaa" won't work, but "aaxaa" would work).
(defun test1-p (string)
  (do* ((s 0 (1+ s))
	(e (+ s 2) (+ s 2)))
       ((= s (- (length string) 3)))
    (let ((fragment (subseq string s e)))
      (if (search fragment string :test #'equalp :start2 e)
	  (return t)))))

;; Test 2: Contains at least one letter that repeats with exactly one letter between them
;;         (e.g. "xyx", "abcdefeghi", "aaa".
(defun test2-p (string)
  (do* ((s 0 (1+ s))
	(e (+ s 3) (+ s 3)))
       ((= s (- (length string) 2)))
   (let ((fragment (subseq string s e)))
      (if (equalp (aref fragment 0) (aref fragment 2))
	 (return t)))))

;; String is nice if it satisfies the above two tests.
(defun is-nicer-p (string)
  (and (test1-p string)
       (test2-p string)))

;; Count the nicer strings
(defun count-nicer-strings (file)
  (with-open-file (myfile file)
    (loop :for line = (read-line myfile nil nil)
	  :while line
	  :counting (is-nicer-p line) :into nice-strings-count
	  :finally (return nice-strings-count))))
