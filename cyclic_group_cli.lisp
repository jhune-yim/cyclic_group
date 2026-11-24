;;; Find all primitive roots of the multiplicative group of units modulo n.
;;; Run from the directory containing this script.
;;;   sbcl --script cyclic_group_cli.lisp

(defpackage #:cyclic-group
  (:use #:cl)
  (:export #:primitive-roots #:check-group))

(in-package #:cyclic-group)

(defun remove-factor (n factor)
  "Divide the positive integer N by FACTOR until it is no longer divisible."
  (loop
    (multiple-value-bind (quotient remainder) (truncate n factor)
      (unless (zerop remainder)
        (return n))
      (setf n quotient))))

(defun distinct-prime-factors (n)
  "Return the distinct prime factors of the positive integer N."
  (let ((factors nil))
    (loop for divisor = 2 then (if (= divisor 2) 3 (+ divisor 2))
          while (<= (* divisor divisor) n)
          when (zerop (rem n divisor))
            do (push divisor factors)
               (setf n (remove-factor n divisor)))
    (when (> n 1)
      (push n factors))
    (nreverse factors)))

(defun cyclic-unit-group-order (n)
  "Return the order of U(N) when it is cyclic and N >= 2, or NIL otherwise."
  (check-type n integer)
  ;; For n >= 2, U(n) is cyclic exactly when n is 2, 4, p^k, or 2*p^k,
  ;; where p is an odd prime and k >= 1.
  (cond
    ((< n 2) nil)
    ((= n 2) 1)
    ((= n 4) 2)
    ((zerop (rem n 4)) nil)
    (t
     (let* ((odd-part (if (evenp n) (ash n -1) n))
            (prime (loop for divisor from 3 by 2
                         while (<= (* divisor divisor) odd-part)
                         when (zerop (rem odd-part divisor))
                           return divisor
                         finally (return odd-part))))
       (when (= (remove-factor odd-part prime) 1)
         ;; The odd prime power and its double have the same totient.
         (- odd-part (truncate odd-part prime)))))))

(defun mod-exp (base exponent modulus)
  "Raise BASE to a nonnegative EXPONENT modulo a positive MODULUS."
  (if (zerop exponent)
      (mod 1 modulus)
      (loop with result = 1
            for factor = (mod base modulus) then (mod (* factor factor) modulus)
            do (when (oddp exponent)
                 (setf result (mod (* result factor) modulus)))
               (setf exponent (ash exponent -1))
            until (zerop exponent)
            finally (return result))))

(defun find-primitive-root (n order)
  "Find the smallest primitive root of the cyclic group U(N)."
  ;; A unit has order ORDER if none of these powers equals 1 modulo n.
  (let ((exponents (mapcar (lambda (prime) (truncate order prime))
                           (distinct-prime-factors order))))
    (loop for candidate from 1 below n
          when (and (= (gcd candidate n) 1)
                    (every (lambda (exponent)
                             (/= (mod-exp candidate exponent n) 1))
                           exponents))
            return candidate
          finally (error "No primitive root found for cyclic modulus ~D." n))))

(defun primitive-roots (n)
  "Return the sorted primitive roots modulo N and the order of U(N).
Return NIL for both values if N < 2 or U(N) is not cyclic."
  (let ((order (cyclic-unit-group-order n)))
    (if order
        (let ((root (find-primitive-root n order)))
          ;; The order of root^k is order / gcd(k, order). Each next power
          ;; takes one modular multiplication.
          (values (sort (loop for k from 1 to order
                              for power = root then (mod (* power root) n)
                              when (= (gcd k order) 1)
                                collect power)
                        #'<)
                  order))
        (values nil nil))))

(defun check-group (n)
  "Print whether U(N) is cyclic and list its generators when it is."
  (format t "===== n = ~D =====~%" n)
  (multiple-value-bind (roots order) (primitive-roots n)
    (cond
      (order
       (format t "U(~D) is cyclic.~%" n)
       (format t "Euler's totient phi(~D) = ~D~%" n order)
       (format t "Number of generators: ~D~%" (length roots))
       (format t "Primitive roots: ~S~%~%" roots))
      ((< n 2)
       (format t "The modulus must be at least 2.~%~%"))
      (t
       (format t "U(~D) is not cyclic.~%~%" n)))))

(defun parse-modulus (text)
  "Return the decimal integer in TEXT if it is at least 2, or NIL otherwise."
  (handler-case
      (let ((n (parse-integer text :radix 10)))
        (when (>= n 2)
          n))
    (parse-error () nil)))

(defun process-modulus (text)
  "Print the result for TEXT and return true if it contains a valid modulus."
  (let ((n (parse-modulus text)))
    (cond
      (n
       (check-group n)
       t)
      (t
       (format *error-output*
               "Invalid modulus ~S. Enter a decimal integer of at least 2.~%"
               text)
       nil))))

(defun interactive-loop ()
  "Read one modulus per line until the user enters Q or reaches end-of-file."
  (loop
    (format t "Enter a modulus n >= 2 (q to quit): ")
    (force-output)
    (let ((line (read-line *standard-input* nil)))
      (unless line
        (terpri)
        (return))
      (when (string-equal (string-trim '(#\Space #\Tab #\Return) line) "q")
        (return))
      (process-modulus line))))

(defun main (args)
  "Run the requested mode and return the process exit status."
  (cond
    ((null args)
     (interactive-loop)
     0)
    ((equal args '("--demo"))
     (dolist (n '(7 10 15 18))
       (check-group n))
     0)
    (t
     (let ((status 0))
       (dolist (arg args status)
         (unless (process-modulus arg)
           (setf status 1)))))))

;; Under sbcl --script, the arguments follow the executable in *posix-argv*.
(sb-ext:exit :code (main (cdr sb-ext:*posix-argv*)))
