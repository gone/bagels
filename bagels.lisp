(defvar target 0)
(defvar guess 0)

(defun =s (&rest equals)
  (let ((test (car equals)))
	(loop for n in (cdr equals) always (= n test)))) 

(defmacro while (test &body body)
  `(do ()
	   ((not ,test))
	 ,@body))

(defun good-guess-p (guess)
  (and (> 1000 guess) (> guess 99)
	   (= 3 (length (adjoin (truncate guess 100)
							 (adjoin (truncate (mod guess 100) 10)
									 (list (mod guess 10))))))))

(defun get-and-save-random ()
  (setq target (random 1000))
  (while (not (good-guess-p target))
	(setq target (random 1000))))
  
(defun guess ()
  (format 't "Give me a number~%")
  (let ((x (read)))
	(when (equal x "quit")
		(quit))
	(if (not (good-guess-p x))
		(progn (format 't "NOT A GOOD GUESS - 3 NUMBERS, NO REPEATS!~%")
			   (guess))
	  x)))

(defmacro numbers-as-string ((&rest numbers) &body body)
  `(let ,(loop for number in numbers collecting `(,number (write-to-string ,number)))
	,@body))

(defun compare-guess (guess target)
  (let ((goods ()))
	(numbers-as-string (guess target)
					   (dotimes (i 3) 
						 (let ((digit (char guess i)))
						   (cond ((equal digit(char target i)) (push "Fermi" goods)) 
								 ((find digit target) (push "Pico" goods))))))
	goods))


(defun bagels ()
  (get-and-save-random)
  (while (and (setq guess (guess)) (not (= guess target)))
	(let ((results (compare-guess guess target)))
	  (if (not results)
		  	  (format 't "BAGELS~%")
		(format 't "~{~a  ~}~%" results))))
  (format 't "You got it!"))

(defconstant intro-text
  "Welcome to Bagels! Game created by the Lawrence Hall of Science and Math in Berkley ~%
This version Created by Ben Beecher in 2010~%")

(defun intro ()
  (format 't intro-text))


(defun another-or-end ()
  (format 't "That was fun! Another?")
  (yes-or-no-p))

(defun end ()
  (format 't "Thanks for playing! ~%"))

(defun bagels-game ()
  (intro)
  (bagels)
  (while (another-or-end)
	(bagels))
  (end))