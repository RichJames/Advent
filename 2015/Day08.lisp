;;;;
;;;; Advent of Code 2015
;;;   Day 8


;; Escape sequences used:
;;   - \\ (represents a single backslash)
;;   - \" (represents a single doulbe-quote character)
;;   - \x## (represents a single character with that ASCII code (##)

;; Problem:
;;  Compute the difference between the number of literal characters
;;  in the file and the number of characters they actually represent.
;;
;;  Example:
;;    ""          (two literal characters, represent zero characters)
;;    "abc"       (5 literal characters, represent 3 characters)
;;    "aaa\"aaa"  (10 literal characters, represent 7 actual characters)
;;    "\x27"      (6 literal characters, represent a single character(an apostrophe)
;;
;;  Ignore all whitespace.
;;
;;  Note: after inspecting the input data, I don't need to worry about the case where
;;  there might be a sequence like "\\x21".  In all cases where something similar to
;;  this exists, it actually will have a sequence like "\\\x21", which will be
;;  interpreted as the literal string "\!".

(defparameter *whitespace* '(#\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout #\Space #xA))

(defun ws-p (c)
  (member c *whitespace*))

(defun count-bytes (file)
  (with-open-file (s file
		     :element-type 'unsigned-byte)
    (loop :for c = (read-byte s nil nil)
	  :while c
	  :unless (ws-p c)
	    :collect c)))

(defun count-all-bytes (file)
  (with-open-file (s file
		     :element-type 'unsigned-byte)
    (loop :for c = (read-byte s nil nil)
	  :while c
	  :collect c)))

(defun count-chars (char-list &key (char-count 0) (escape nil) (hex-seq nil) (hex-chars nil))
  (let ((c (car char-list))
	(cdr-list (cdr char-list)))
    (cond ((null c) char-count)
	  (hex-chars                     (count-chars cdr-list
						      :char-count (1+ char-count)))
	  ((and hex-seq (not hex-chars)) (count-chars cdr-list
						      :char-count char-count
						      :hex-chars t))
	  ((and escape (eq c 120))       (count-chars cdr-list
						      :char-count char-count
						      :hex-seq t))
	  ((and escape (not hex-seq))    (count-chars cdr-list
						      :char-count (1+ char-count)))
	  ((eq c 92)                     (count-chars cdr-list
						      :char-count char-count
						      :escape t))
	  ((or (eq c 34) (ws-p (code-char c)))       (count-chars cdr-list
						      :char-count char-count))
	  (t                             (count-chars cdr-list
						      :char-count (1+ char-count))))))
(defun tally (file)
  (let ((bytes-list (count-bytes file)))
    (- (length bytes-list) (count-chars bytes-list))))

;;; Part 2
;;;
;;;  Given a string of characters, encode them in a new string.  For example,
;;;    - ""            --> "\"\""
;;;    - "abc"         --> "\"abc\""
;;;    - "aaa\"aaa"    --> "\"aaa\\\"aaa\""
;;;    - "\x27"        --> "\"\\x27\""
;;;
;;;  Basically, inside of quotes, insert the string of charcters, but use \ to
;;;  escape quotes and backslashes.
;;;
;;;  The task is to find the total number of characters needed to encode the new
;;;  string minus the number of characters of the code in each original string
;;;  literal.
;;;
;;;  For example, with the strings above, this would be
;;;   (6 + 9 + 16 + 11 = 42) minus 23 (as we computed in part 1.)
;;;
;;;  Note, every character string will be surrounded by a pair of
;;;  "\"<encoded string>\"".  Another way to look at it:
;;;   - Start the string with "
;;;   - If the source string has a quote, insert \" in the new string
;;;   - If the source string has a \, insert \\ in the new string.
;;;   - Finish the string with ".
;;;  This can be done character-by-character while walking the source string.

(defun count-encoded-chars (char-list &key (char-count 0))
  (let ((c (car char-list))
	(cdr-list (cdr char-list)))
    (cond ((null c) char-count)
	  ((or (eq c 34)
	       (eq c 92)
	       (eq c 10)) (count-encoded-chars cdr-list :char-count (+ char-count 2)))
	  (t (count-encoded-chars cdr-list :char-count (1+ char-count))))))

(defun tally2 (file)
  (let* ((char-count (length (count-bytes file)))
	 (encoded-count (count-encoded-chars (count-all-bytes file)))
	 (tally (- encoded-count char-count)))
    (format t "~%encoded-count = ~d, char-count = ~d, tally = ~d"
	    encoded-count
	    char-count
	    tally)))
