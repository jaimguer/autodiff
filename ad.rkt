#lang racket/base

(require (rename-in racket/base
                    (+    r+)
                    (-    r-)
                    (*    r*)
                    (/    r/)
                    (exp  rexp)
                    (sin  rsin)
                    (cos  rcos)
                    (log  rlog)
                    (sqrt rsqrt)
                    (expt rexpt)))

(provide dual dual-x dual-dx
         r+ r- r* r/ rexp rsin rcos rlog rsqrt
         +  -  *  /  exp  sin  cos  log  sqrt
         recip const var dlift )

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
  (case-lambda
    [(d) (when (number? d) (set! d (const d)))
         (make-dual (r* -1 (dual-x d)) (r* -1 (dual-dx d)))]
    [(d1 d2)
     (when (number? d1) (set! d1 (const d1)))
     (when (number? d2) (set! d2 (const d2)))
     (let ([x (dual-x  d1)]
           [a (dual-dx d1)]
           [y (dual-x  d2)]
           [b (dual-dx d2)])
       (make-dual (r- x y)
                  (r- a b)))]))

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
        (make-dual (func x) (r* a (deriv x)))))))

(define exp (dlift rexp rexp))

(define sin (dlift rsin rcos))

(define negsin
  (lambda (x)
    (r* -1 (rsin x))))

(define cos
  (dlift rcos (lambda (x)
                (r- (rsin x)))))

;;; Natural logarithm
(define log (dlift rlog (lambda (x) (r/ 1 x))))

(define sqrt
  (dlift rsqrt (lambda (x)
                 (r/ 0.5 (rsqrt x)))))

;;; Because this program implements an algebra of dual numbers, the returned
;;; value of dlift must itself be a dual.  Consequently, to
