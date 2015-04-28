#lang racket/base

(require (rename-in racket/base
	 	    (+ r+)
		    (- r-)
		    (* r*)
		    (/ r/)
		    (exp rexp)
		    (sin rsin)
		    (cos rcos)
		    (log rlog)))

(provide + - * negate const var dlift exp sin cos log)

;; Code based off of Jerzy Karczmarczuk's Haskell implementation,
;; found in Functional Differentation of Computer Programs
;; https://karczmarczuk.users.greyc.fr/arpap/diffalg.pdf

;; Thanks to Tim Zakian for the paper suggestion.

(define-struct dual [x dx])

;;; + :: Dual -> Dual -> Dual (+ d1 d2) returns a pair
;;; with the components of d1 and d2 added together
(define +
  (lambda (d1 d2)
    (when (number? d1) (set! d1 (const d1)))
    (when (number? d2) (set! d2 (const d2)))
    (let ([x (dual-x  d1)]
          [a (dual-dx d1)]
          [y (dual-x  d2)]
          [b (dual-dx d2)])
      (make-dual (r+ x y) (r+ a b)))))

;;; - :: Dual -> Dual -> Dual
;;; (- d1 d2) returns a dual with the components of d1 and d2 subtracted
(define -
  (lambda (d1 d2)
    (when (number? d1) (set! d1 (const d1)))
    (when (number? d2) (set! d2 (const d2)))
    (let ([x (dual-x  d1)]
          [a (dual-dx d1)]
          [y (dual-x  d2)]
          [b (dual-dx d2)])
    (make-dual (r- x y )
               (r- a b)))))

;;; * :: Dual -> Dual -> Dual
;;; (* d1 d2) creates a dual with the x's multiplied, and their derivative
;;; based on the product rule
(define *
  (lambda (d1 d2)
    (when (number? d1) (set! d1 (const d1)))
    (when (number? d2) (set! d2 (const d2)))
    (let ([x (dual-x  d1)]
          [a (dual-dx d1)]
          [y (dual-x  d2)]
          [b (dual-dx d2)])
    (make-dual (r* x y)
               (r+ (r* x b)
                   (r* a y))))))

;;; negate :: Dual -> Dual
;;; (negate d) changes the sign of d's components
(define negate
  (lambda (d)
    (when (number? d) (set! d (const d)))
    (make-dual (r* -1 (dual-x d)) (r* -1 (dual-dx d)))))

;;; / :: Dual -> Dual -> Dual
;;; (/ d1 d2) returns a dual with the derivative calculated according to the
;;; quotient rule
(define /
  (lambda (d1 d2)
    (when (number? d1) (set! d1 (const d1)))
    (when (number? d2) (set! d2 (const d2)))
    (let ([x (dual-x  d1)]
          [a (dual-dx d1)]
          [y (dual-x  d2)]
          [b (dual-dx d2)])
    (make-dual (r/ x y)
               (r/ (r- (r* a y)
                       (r* x b))
                   (r* y y))))))

;;; recip :: Dual -> Dual
(define recip
  (lambda (d)
    (when (number? d) (set! d (const d)))
    (let* ([x (dual-x  d)]
           [a (dual-dx d)]
           [w (r/ 1 x)])
      (make-dual w (r* (r* -1 a) w w)))))

;;; Know when a constant is being provided
(define const
  (lambda (n)
    (make-dual n 0.0)))

;; Know when we wish to find the derivative of a function
(define var
  (lambda (n)
    (make-dual n 1.0)))

;;; Heart of the derivative calculation.  This is the application of the chain
;;; rule
(define dlift
  (lambda (func deriv)
    (lambda (d)
      (when (number? d) (set! d (const d)))
      (let ([x (dual-x  d)]
            [a (dual-dx d)])
	(values (func x)
		(r* a (deriv x)))))))

(define exp
  (dlift rexp rexp))

(define sin
  (dlift rsin rcos))

(define negsin
  (lambda (x)
    (r* -1 (rsin x))))

(define cos
  (dlift rcos (lambda (x)
                (r* -1 (rsin x)))))

(define log
  (dlift rlog (lambda (x) (r/ 1 x))))


;; Example programs
;; First number is the function value at that point, second value is
;; the derivative at said point.

;; To find the function value with no derivatives, proceed as you always would.
;; > (define f (lambda (x) (sin x)))
;; > (f 1)
;; 0.8414709848078965
;; 0.0

;; To get the derivative as well, simply replace the 1 with (var 1)
;; > (f (var 1))
;; 0.8414709848078965
;; 0.5403023058681398

;; Limitations: this does not work on functions of multiple
;; variables. From what I can tell, it would involve using the
;; reverse-mode form of autodiff.
