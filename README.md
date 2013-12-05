#Shunting-Yard & RPN

Implementation of the Shunting-Yard algorithm and RPN in LISP

#How to use

(load "sy.lisp")
(trace sy) ;to see every steps
(shunting-yard '(10 #\+ 20 #\/ 4))
(rpn '(10 20 4 #\/ #\+))


