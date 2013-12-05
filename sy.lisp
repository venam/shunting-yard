;;;COPYRIGHT AND PERMISSION NOTICE
;;;
;;;Copyright (c) 2013, Patrick Louis 
;;;
;;;All rights reserved.
;;;
;;;Redistribution and use in source and binary forms, with or without
;;;modification, are permitted provided that the following conditions are met:
;;;
;;;    1.  The author is informed of the use of his/her code. The author does not have to consent to the use; however he/she must be informed.
;;;    2.  If the author wishes to know when his/her code is being used, it the duty of the author to provide a current email address at the top of his/her code, above or included in the copyright statement.
;;;    3.  The author can opt out of being contacted, by not providing a form of contact in the copyright statement.
;;;    4.  If any portion of the author's code is used, credit must be given.
;;;            a. For example, if the author's code is being modified and/or redistributed in the form of a closed-source binary program, then the end user must still be made somehow aware that the author's work has contributed to that program.
;;;            b. If the code is being modified and/or redistributed in the form of code to be compiled, then the author's name in the copyright statement is sufficient.
;;;    5.  The following copyright statement must be included at the beginning of the code, regardless of binary form or source code form.
;;;
;;;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;;;ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;Except as contained in this notice, the name of a copyright holder shall not
;;;be used in advertising or otherwise to promote the sale, use or other dealings
;;;in this Software without prior written authorization of the copyright holder.
;;;
;;;
;;;
;;; Implementation of the Shunting-Yard algorithm in lisp
;;;

;; Example:
;; (10 #\+ 20 #\* #\( 30 #\/ 40 #\))
;; which is:
;; 10 + 20 * ( 30 / 40 )
;; That becomes the RPN:
;; 10 20 30 40 / * +
;; 25

;;
;; Check if the char is an operator
;;

(defun isoperator (o1)
	(if (and 
		  (characterp o1)
		  (or 
			(char= o1 #\+) 
			(char= o1 #\-)
			(char= o1 #\/)
			(char= o1 #\*)
			(char= o1 #\^)))
	  T
	  nil))

;;
;; pull out of the stack into output until find the left parenthesis
;;
;;
;; ( INPUT OUTPUT STACK)
;;

(defun untilleftparenthesis (INPUT OUTPUT STACK)
  (cond
	; if the stack is empty return nil, unmatched parenthesis
	((if (null STACK) nil))
	; if the left parenthesis is found in the stack return it without it
	((if (and
		   (character (first STACK))
		   (char= (first STACK) #\())
	   (list INPUT  OUTPUT  (rest STACK) )
	   ))
	(T (untilleftparenthesis
							INPUT
							(cons (first STACK) OUTPUT)
							(rest STACK)))
	))

;;
;; Remove everything from the stack and put it in the output
;;

(defun removefromstack (OUTPUT STACK)
  (if (null STACK)
	OUTPUT
	(removefromstack 
	  (cons (first STACK) OUTPUT)
	  (rest STACK))))

;; ^    (1)
;; * /  (2)
;; + -  (3)
(defun checkprecedence (INPUT OUTPUT STACK)
  (cond
	; if stack is empty put the operator on the stack
	((if (or (null STACK)
			 (char= (first STACK) #\()
			 (char= (first STACK) #\)))
	   (list (rest INPUT)   OUTPUT  (cons (first INPUT) STACK))))
	; if the top of the stack is ^ then there's nothing with more precedence
	((if (char= (first STACK) #\^)
	   (checkprecedence INPUT (cons (first STACK) OUTPUT) (rest STACK))
	   ))
	; if there's no more ^ in the stack and the operator is ^
	((if (char= (first INPUT) #\^)
		(list (rest INPUT)  OUTPUT  (cons (first INPUT) STACK)  )))
	; if the operator is either * / and the top of the stack is either + -
	((if (and
		   (or (char= (first INPUT) #\*) (char= (first INPUT) #\/))
		   (or (char= (first STACK) #\+) (char= (first INPUT) #\-))
		   )
	   (list (rest INPUT)  OUTPUT  (cons (first INPUT) STACK))
	   ))
	; else
	; meaning (* /) over (* /) or (+ -) over (* / - +)
	(T (checkprecedence INPUT (cons (first STACK) OUTPUT) (rest STACK)))
	))


(defun SY (INPUT OUTPUT STACK)
  (cond
	; When there are no more tokens to read:
	((if (null INPUT)   
	   (let* (
			  (N (removefromstack OUTPUT STACK)))
		 (reverse N)
		 )))

	; if the token is a number, then add it to the output queue.
	((if (or 
		   (numberp (first INPUT))
		   (char= (first INPUT) #\x)
		   (char= (first INPUT) #\X)
		   (char= (first INPUT) #\Y)
		   (char= (first INPUT) #\y))
	   (let* (
			  (NOUT (cons (first INPUT) OUTPUT)))
		 (SY (rest INPUT) NOUT STACK)))) 
	; if the token is an operator, o1, then:
	((if (isoperator (first INPUT)) (let* (
											;while there is an operator token, o2, 
											;at the top of the stack, and
											(N (checkprecedence INPUT OUTPUT STACK)))
									  (SY (first N) (second N) (third N)) )))
	; if the token is a left parenthesis, then push it onto the stack
	((if (and 
		   (character (first INPUT))
		   (char= (first INPUT) #\())
	   (let* (
			  (NST (cons (first INPUT) STACK)))
		 (SY (rest INPUT) OUTPUT NST))))
	;if the token is a right parenthesis:
	((if (and
		   (character (first INPUT))
		   (char= (first INPUT) #\)))
	   ; Until the token at the top of the stack is a left parenthesis,
	   ; pop operators off the stack onto the output queue.
	   ; pop the left parenthesis from the stack, but not onto the output queue.
	   ; If the stack runs out without finding a left parenthesis, 
	   ; then there are mismatched parentheses
		(let* (
			   (N (untilleftparenthesis (rest INPUT) OUTPUT STACK)))
		  (SY (first N) (second N) (third N))
	   )))
	))

(defun shunting-yard (INPUT)
  (SY INPUT () ()))

;;
;; Reverse Polish Notation Calculation
;;


(defun rpn-aux (INPUT STACK)
  (cond
	; If there is only one value in the stack
	; That value is the result of the calculation.
	((if (null INPUT) 
		(first STACK)
	 ))
	; If the token is a value
	; Push it onto the stack.
	((if (numberp (first INPUT))
	   (rpn-aux (rest INPUT) (cons (first INPUT) STACK))
	   ; Otherwise, the token is an operator
	   ; It is known a priori that the operator takes n arguments.
	   ; if there are fewer than n values on the stack 
	   ; (Error)
	   ; Else, Pop the top n values from the stack.
	   ; Evaluate the operator, with the values as arguments.
	   ; Push the returned results, if any, back onto the stack.
	   (let* (
			  ; N being the new stack
			  (N (calc-operator (first INPUT) STACK)))
		 (rpn-aux (rest INPUT) N))
	   ))))

(defun rpn (INPUT)
  (rpn-aux INPUT ()))

;;
;; ^    operator
;; * /  operators
;; + -  operators
;;

(defun ^ (B E)
  (fast-power-aux B E B))

(defun fast-power-aux (B E A)
  (if (= 1 E)
	B
	(fast-power-aux (* B A) (1- E) A)))

(defun do_calc (STACK FOO)
  (cons (funcall FOO (second STACK) (first STACK)) (rest (rest STACK))))

(defun calc-operator (OPER STACK)
  (cond
	((if (char= OPER #\+)
	   (do_calc STACK #'+)))
	((if (char= OPER #\-)
	   (do_calc STACK #'-)))
	((if (char= OPER #\*)
	   (do_calc STACK #'*)))
	((if (char= OPER #\/)
	   (do_calc STACK #'/)))
	((if (char= OPER #\^)
	   (do_calc STACK #'^)))
	(T nil)))

