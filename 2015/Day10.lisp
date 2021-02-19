rm .bash_history\~
which sqlite3
ls
clear

ls
cd Music/
ls
cd rips
ls
mkdir Harris\,\ Emmylou
ls
mv 2000.Red\ Dirt\ Girl/ Harris,\ Emmylou/
ls
cd Harris,\ Emmylou/
ls
cd 2000.Red\ Dirt\ Girl/
ls
vorbisgaiin -a *
vorbisgain -a *
cd ../..
cd Steve\ Miller\ Band/
ls
cd 2017.Ultimate\ Hits\ Deluxe\ Edition/
ls
mv cover.jpeg ..
ls
vorbisgain -a *
cd
cd Music/
ls
cd ..
ls
cd iPod/
ls
./CLcreateplaylist.sh 
./CLcreateNewMusicPlaylist.sh 
rm ~/.bash_history\~
cd
cd Dropbox/
ls
ls *org
ls
ls *org
cd org
ls
cd .local/share/applications/
ls
ls
cd /usr/share/applications/
ls emacs*
cd /usr/share/applications/
ls emacs*
cd
cd .local/share/applications/
ls
ls emacs*
cd /usr/share/applications/
ls emacs*
cd ../..
cd local/share/applications/
ls
cd /usr/share/applications/
ls
cd
clear
cd .local/share/applications/
ls
clear
xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
clear
cd
cp .emacs .emacs.bak
ls
mv .emacs .emacs.d/
ls .emacs.d/
cd .emacs.d/
mv .emacs init.el
ls
ls
ls
rm settings.el
ls Zet*
ls *Zet*
ls
ls *Z*org
rm 20210107181021-zettlekasten_method.org
cd ../Documents/
ls
mv Thoughts\ on\ Sapiens.odt Books\ and\ Articles/
ls
rm Shiipping\ label\ -\ noah\ birthday.pdf
ls
rm InfiniteEnergy\ Terms\ of\ Service.pdf
rm InfiniteEnergy\ Electricity\ Facts\ Label.pdf
ls
mv Amazon\ Prime\ Credit\ Card\ Agreement.pdf Amazon/
ls
rm Important\ information.odt
rm Jonathan\ Katz.odt
ls
rm org-mode-practice.org
rm org-mode-practice.org\~
ls
ls
mv Futures\ Course\ 2019-07-08\ stats.ods Golf/
ls
mv harmonyone-user-guide.pdf Manuals/
ls
cd Cars
ls
cd ..
ls
mkdir Auto\ Insurance
ls
mv Auto\ insurance\ coverage.odt Auto\ Insurance/
ls
mv Traffic\ Ticket\ PaymentReceipt.pdf Auto\ Insurance/
ls
mv Daniels\ driving\ record\ password\ is\ DL\ number.pdf Auto\ Insurance/
ls
cd ../org
ls
clear
crontab -e
ls
ls -al
rm Day10-sbcl.lisp
;;;;
;;;; Advent of Code 2015


;;;   Day 10

(defpackage #:Day10
  (:use #:cl))

(in-package #:Day10)

;;; ==========  Part 1 ========================

;;; This is just a brute force approach to getting the result.  It will
;;; return the character count after 40 iterations, but it takes a little
;;; while to run.

(defparameter *input* "1113122113")

(defun look-say (input)
  (loop	:with prev-c = nil
	:with c-count  = 0
	:with result = ""
	:for c  :across input
	:when (and (not (equal c prev-c))
		   (not (null prev-c)))
	  :do (progn
		(setf result (concatenate 'simple-string
					  result
					  (write-to-string c-count)
					  (format nil "~a" prev-c)))
		(setf prev-c nil)
		(setf c-count 0))
	:do (progn
	      (incf c-count)
	      (setf prev-c c))
	:finally (return (concatenate 'simple-string
				      result
				      (write-to-string c-count)
				      (format nil "~a" prev-c)))))


(defun part1 (input &optional (reps 1))
  (loop :for i :below reps
	:for result = (look-say input) :then (look-say result)
	:finally (return (length result))))

;;; ============ End of Part1 ====================


;;; ============ Part 2 ==========================

;;; This challenge is harder.  I have to get the character count after 50
;;; iterations.  A naive approach is to just run part1 for 50 iterations.
;;; But this takes forever and, I think, just locks up the lisp REPL.
;;; So the challenge here is to create an efficient way to compute these
;;; values so you don't become resource constrained.


;; This is an attempt to perform the look-say logic using lists of numbers
;; rather than actual numbers and strings.  It isn't efficient enough.

(defparameter *input2* '(1 1 1 3 1 2 2 1 1 3))

(defun look-say2 (input)
  (do* ((seq input)
	(1st (car seq) (car seq))
	(2nd (cadr seq) (cadr seq))
	(3rd (caddr seq) (caddr seq))
	(result nil))
       ((null seq) result)
    (cond ((and (eq 1st 2nd)
		(eq 2nd 3rd)) (progn
				(setf result (append result (list '3 1st)))
				(setf seq (cdddr seq))))
	  ((eq 1st 2nd) (progn
			  (setf result (append result (list '2 1st)))
			  (setf seq (cddr seq))))
	  (t (progn
	       (setf result (append result (list '1 1st)))
	       (setf seq (cdr seq)))))))

(defun part1-o (input &optional (reps 1))
  (loop :for i :below reps
	:for result = (look-say2 input) :then (look-say2 result)
	:finally (return result)))

;; Here I try the same as above, but use nconc, thinking that maybe the problem
;; is the constant need to allocate ever increasing numbers of cons cells.  It
;; still isn't efficient enough.

(defun look-say3 (input)
  (do* ((seq input)
	(1st (car seq) (car seq))
	(2nd (cadr seq) (cadr seq))
	(3rd (caddr seq) (caddr seq))
	(result nil))
       ((null seq) result)
    (cond ((and (eq 1st 2nd)
		(eq 2nd 3rd)) (progn
				(setf result (nconc result (list '3 1st)))
				(setf seq (cdddr seq))))
	  ((eq 1st 2nd) (progn
			  (setf result (nconc result (list '2 1st)))
			  (setf seq (cddr seq))))
	  (t (progn
	       (setf result (nconc result (list '1 1st)))
	       (setf seq (cdr seq)))))))

(defun part1-o2 (input &optional (reps 1))
  (loop :for i :below reps
	:for result = (look-say3 input) :then (look-say3 result)
	:finally (return (length result))))

;; This is an attempt to measure the performance difference between the original
;; part1 and the "optimized" part1, using one of the two approaches above.  The
;; results were inconclusive.  Repeated tests gave varying results.
(defun test-o (reps)
  (setf *input2* '(1 1 1 3 1 2 2 1 1 3))
  (progn
    (time (part1 *input* reps))
    (time (part1-o2 *input2* reps))))

;; This is an attempt to get the part2 answer by separately computing each
;; half of the input.  It still is too slow.

(defparameter *left* "11131")
(defparameter *right* "22113")

(defun part2 (left right &optional (reps 1))
  (concatenate 'string
	       (part2-o left reps)
	       (part1 right reps)))

;; These are test functions so I can see if the different approaches
;; I take produce the same results.
(defun display-3 (input &optional (reps 3))
  (loop :for i :below reps
	:when (= i 0)
	  :do (format t "~100:@<~a~>~%" input)
	:when (> i 0)
	  :do (format t "~100:@<~a~>~%" (part1 input i))))

(defun display-3o (input &optional (reps 3))
  (loop :for inlist = input :then input
	:for i :below reps
	:when (= i 0)
	  :do (format t "~100:@<~a~>~%" inlist)
	:when (> i 0)
	  :do (format t "~100:@<~a~>~%" (part1-o2 inlist i))))


;;************************** Trying to use atoms *****************************

;; The original input is one of 92 atoms identified by John Conway.  These atoms recur as
;; these sequences grow while doing iterations of look-say.  You should be able to concatenate
;; subsets of these atoms to create the final result.

;; Atom data is from:
;; http://www.nathanieljohnston.com/2010/10/a-derivation-of-conways-degree-71-look-and-say-polynomial/

(defstruct ls-atom
  number
  subsequence
  length
  evolves-to)

(defparameter *atoms* (make-array 92 :fill-pointer 0 :element-type 'ls-atom))

;; The atom data is in a file called day10atoms.txt
(defun load-data (file)
  (with-open-file (myfile file)
    (loop :for nbr = (read myfile nil nil)
	  :while nbr
	  :for seq = (read myfile nil nil)
	  :for len = (read myfile nil nil)
	  :for evo = (read myfile nil nil)
	  :do (vector-push (make-ls-atom :number nbr
					 :subsequence (write-to-string seq)
					 :length len
					 :evolves-to evo)
			   *atoms*))))

;; Returns NIL if str doesn't start with a known atom.  Otherwise, the longest atom
;; that it starts with will be returned.
;; *NOTE* By using find-if, we are doing a sequential search over the array of atoms.
;;  In cases where there is no match, this means we will traverse the array fully
;;  each time.  I don't know if this is a significant impact on my performance though.
;;  It could be interesting to develop a binary search version and see if it can
;;  produce a better result.
(defun atom-match (str)
  (find-if #'(lambda (subseq)
	       (let ((res (search subseq str))) ; search for subseq in str
		 (and res (= res 0))))
	   *atoms*
	   :key #'ls-atom-subsequence
	   :from-end t))

;; Recursive look-say algorithm, using atoms.  Runs out of stack space when iterated
;; 50 times.  For SBCL, the default control-stack-size is 2 megabytes.  This can be
;; altered by launching SBCL like this:
;;   sbcl --control-stack-size 10
;; where 10 means 10 megabytes.  I don't know how to launch sbcl in slime with a
;; different stacksize, however. Possibly in SBCL's config file.
(defun look-say-atom (nbr)
  (cond ((null nbr) nil)
	((numberp nbr) (ls-atom-evolves-to (elt *atoms* (1- nbr))))
	((listp nbr) (append (look-say-atom (car nbr)) (look-say-atom (cdr nbr))))))

;; Tail recursive look-say algorithm, using atoms.  But, runs very slowly at high
;; iterations.
(defun look-say-atom (nbr &optional (result nil))
  (cond ((null nbr) result)
	((listp nbr) (look-say-atom (cdr nbr)
				    (append result (ls-atom-evolves-to (elt *atoms* (1- (car nbr)))))))))

(defun look-say-atom (lst)
  (mapcan #'(lambda (x)
	      (ls-atom-evolves-to (elt *atoms* (1- x))))
	  lst))

(defun add-atoms (lst)
  (cond ((null lst) 0)
	(t (+ (ls-atom-length (elt *atoms* (1- (car lst))))
	      (add-atoms (cdr lst))))))

;; To test atom approach, run this function, providing the sequence we want to look-say against.
;;
;; You need to run load-data first, in order to pull in the atom data into the *atoms* array.
(defun part2-a (input &optional (reps 1))
  (loop :with initial-atom = (list (ls-atom-number (atom-match input)))
	:for i :below reps
	:for result = (look-say-atom initial-atom) :then (look-say-atom result)
	:finally (return (add-atoms result))))


;; Here is a binary search algorithm (from Programming Algorithms) that I can adapt for this problem.

(defun bin-search (val vec &key (less '<) (test '=) (key 'identity))
  (when (plusp (length vec))
    (let ((beg 0)
          (end (1- (length vec))))
      (do ()
          ((= beg end))
        (let ((mid (+ beg (floor (- end beg) 2))))
          (if (funcall less (funcall key (aref vec mid)) val)
              (setf beg (1+ mid))
              (setf end mid))))
      (values (aref vec beg)
              beg
              (funcall test (funcall key (aref vec beg)) val)))))

;; Here is an example call of bin-search against my *atoms* array
(bin-search "1113" *atoms* :less 'string< :test 'string= :key 'ls-atom-subsequence)

;; For ease of use, I created a helper function for this application:
(defun bin-search-atoms (val)
  (bin-search val *atoms* :less 'string< :test 'string= :key 'ls-atom-subsequence))

;; This function duplicates atom-match by returning only the found atom or NIL.
(defun atom-match-bin (val)
  (multiple-value-bind (atom notused found)
      (bin-search-atoms val)
    (declare (ignore notused))
    (if found atom)))

;; A version of part2-a that uses the binary search approach
(defun part2-a-bin (input &optional (reps 1))
  (loop :with initial-atom = (list (ls-atom-number (atom-match-bin input)))
	:for i :below reps
	:for result = (look-say-atom initial-atom) :then (look-say-atom result)
	:finally (return (add-atoms result))))


(bin-search "3112112" *atoms* :less 'string< :test 'token-match :key 'ls-atom-subsequence)

(defun token-match (token str)
  (let ((res (search token str)))
    (= res 0)))

;;; The thing I am trying to figure out is if I can make a binary search that finds the largest
;;; atom in the atoms array that matches the start of some string I provide.
;;; Also, is such a thing actually useful?  Does it improve performance?

;;; Where else am I running into performance bottlenecks?

;;; I think the binary search approach is a waste of time.  It is not well suited for doing a fuzzy
;;; search for longest tokens that appear at the start of a string of characters.

;;; My atom logic needs more work.  It seems to work well with the *left* string, but not the *right* string.
;;; I think this is due to the fact that the left string always will be composed of atoms as it evolves,
;;; while the right string is not an atom, nor will it evolve to be atoms.
