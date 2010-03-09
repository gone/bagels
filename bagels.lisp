(defvar num 0) 

(defun bagles ()
  (get-and-save-random)
  (while (and (setq x (guess)) (not (= x num)))
	(let ((results (compare-guess x num)))
	  (format 't "~{~a  ~}" results))))

(defun get-and-save-random ()
  (setq num (random 1000))
  (while (not (good-guess-p number))
	(setq num (random 1000))))
  
(defun guess ()
  (format 't "Give me a number~%")
  (let ((x (read)))
	(if (not (good-guess-p x))
		(progn (format 't "NOT A GOOD GUESS - 3 NUMBERS, NO REPEATS!~%")
			   (guess))
	  x)))

(defun compare-guess (guess target)
  (let (goods)
	(with-numbers-as-string guess target
							(dotimes (i 3) 
							  (let ((letter (position i guess)))
								(cond ((= letter (position i target) (push "Fermi" goods))) 
									  ((find letter target) (push "Pico" goods))))))
	goods))
								

		
(defmacro with-numbers-as-string ((&rest numbers) &body body)
  `(let ,(loop for number in numbers collecting `(,number (write-to-string ,number)))
	,@body))


(defun good-guess-p (guess)
  (and (> 1000 guess) (> guess 99)
	   (not (=s (truncate guess 100)
				(truncate (mod guess 100) 10)
				(mod guess 10)))))

(defun =s (&rest equals)
  (let ((test (car equals)))
	(loop for n in (cdr equals) always (= n test)))) 
