(library (Autodiff ad)
         (export dual + - * negate const var dlift exp sin cos log)
         (import
           (rename
             (chezscheme)
             (+ s+)
             (- s-)
             (* s*)
             (/ s/)
             (exp sexp)
             (sin ssin)
             (cos scos)
             (log slog)))

;;; Record representing a dual number
;;; The x component is the function's value at a given point, while dx is the
;;; derivative's value at the same point
(define-record dual [x dx])

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
      (make-dual (s+ x y) (s+ a b)))))

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
    (make-dual (s- x y )
               (s- a b)))))

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
    (make-dual (s* x y)
               (s+ (s* x b)
                   (s* a y))))))

;;; negate :: Dual -> Dual
;;; (negate d) changes the sign of d's components
(define negate
  (lambda (d)
    (when (number? d) (set! d (const d)))
    (make-dual (s* -1 (dual-x d)) (s* -1 (dual-dx d)))))

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
    (make-dual (s/ x y)
               (s/ (s- (s* a y)
                       (s* x b))
                   (s* y y))))))

;;; recip :: Dual -> Dual
(define recip
  (lambda (d)
    (when (number? d) (set! d (const d)))
    (let* ([x (dual-x  d)]
           [a (dual-dx d)]
           [w (s/ 1 x)])
      (make-dual w (s* (s* -1 a) w w)))))

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
      (if (number? d) (set! d (const d)))
      (let ([x (dual-x  d)]
            [a (dual-dx d)])
        (make-dual (func x)
                   (s* a (deriv x)))))))

(define exp
  (dlift sexp sexp))

(define sin
  (dlift ssin scos))

(define negsin
  (lambda (x)
    (s* -1 (ssin x))))

(define cos
  (dlift scos (lambda (x)
                (s* -1 (ssin x)))))

(define log
  (dlift slog (lambda (x) (s/ 1 x))))


)

