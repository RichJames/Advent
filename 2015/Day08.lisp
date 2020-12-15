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

(with-open-file (s "/home/rich/quicklisp/local-projects/rich/advent/2015/matchsticks.txt"
			    :element-type 'unsigned-byte)
  (dotimes (i 10)
    (let ((c (read-byte s t)))
      (if (not (ws-p c))
	  (format t "~%>~x<" c)))))


(map 'string #'(lambda (c) (print c)) "byc\x9dyxuafof\\\xa6uf\\axfozomj\\olh\x6a")

(defun count-bytes (file)
  (with-open-file (s file
		     :element-type 'unsigned-byte)
    (loop :for c = (read-byte s nil nil)
	  :while c
	  :unless (ws-p c)
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
