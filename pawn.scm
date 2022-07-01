;;;; pawn.scm
;;;;
;;;; Copyright (c) Stewart J. Milberger, 2022
;;;; All rights reserved. No copying.
;;;;

;;; Example 1 from:
;;;
;;;   V. Shapiro (2007). Semi-analytic geometry with R-functions.
;;;   Acta Numerica, 16, pp 239-303
;;;
;;;   https://engrspatial.wiscweb.wisc.edu/wp-content/uploads/sites/715/2014/04/2007-1.pdf

;;;
;;; Demonstrates use of R-functions
;;;
;;; Notes:
;;; o The sign convention in the paper is negated from Clover.  Zero
;;;   and positive values are inside the shape, unlike Clover.
;;;
;;; o The 'omega*' primitives are defined using an algebraic distance
;;;   measure rather than geometric (Euclidian) distance.

(use-math! 'generic)

;;; R-functions for CSG ops
(define (union-dist-r s-dfn . rest-dfns)
  ;;; binary arguments, alpha = 0
  (define (conjunction d1 d2)
    (+ d1 d2 (sqrt (+ (square d1) (square d2)))))
  ;;
  (lambda (pt)
    (let ((s-d (s-dfn pt))
          (rest-d (map (lambda (e) (e pt)) rest-dfns)))
      (fold-left conjunction s-d rest-d))))
(define-shape (union-r s . rest)
              "Shape: Union (R-function based)"
              (default-dist . union-dist-r))
;;;
(define (intersection-dist-r s-dfn . rest-dfns)
  ;;; binary arguments, alpha = 0
  (define (disjunction d1 d2)
    (- (+ d1 d2) (sqrt (+ (square d1) (square d2)))))
  ;;
  (lambda (pt)
    (let ((s-d (s-dfn pt))
          (rest-d (map (lambda (e) (e pt)) rest-dfns)))
      (fold-left disjunction s-d rest-d))))
(define-shape (intersection-r s . rest)
              "Shape: Intersection (R-function based)"
              (default-dist . intersection-dist-r))

;;; Table 4.1(b)
(define (omega1-dist) ; solid of revolution
  (lambda (pt)
    (let ((x (vector-ref pt 0)) (y (vector-ref pt 1)) (z (vector-ref pt 2)))
      (+ (- z)
         (* (/ 7 16) (square (- (sqrt (+ (square x) (square y))) 4)))))))
(define-shape (omega1) "Shape: Solid of Revolution"
              (default-dist . omega1-dist))

(define (omega2-dist) ; cylinder
  (lambda (pt)
    (let ((x (vector-ref pt 0)) (y (vector-ref pt 1)))
      (- 9 (square x) (square y)))))
(define-shape (omega2) "Shape: Cylinder" (default-dist . omega2-dist))

(define (omega3-dist) ; horizontal slab
  (lambda (pt)
    (let ((z (vector-ref pt 2)))
      (* z (- 7 z)))))
(define-shape (omega3) "Shape: Horizontal slab" (default-dist . omega3-dist))

(define (omega4-dist) ; sphere
  (lambda (pt)
    (let ((x (vector-ref pt 0)) (y (vector-ref pt 1)) (z (vector-ref pt 2)))
      (- 1 (square x) (square y) (square (- 7 z))))))
(define-shape (omega4) "Shape: Sphere" (default-dist . omega4-dist))

(define (omega5-dist) ; ellipsoid
  (lambda (pt)
    (let ((x (vector-ref pt 0)) (y (vector-ref pt 1)) (z (vector-ref pt 2)))
      (- 2 (square x) (square y) (* 9 (square (- 6 z)))))))
(define-shape (omega5) "Shape: Ellipsoid" (default-dist . omega5-dist))

(use-math! 'number)

;;;
;;; A chess pawn, the thing we are modeling...
;;;
(define (omega)
  (inverse ; negative space of the shape in the paper (see "Notes" above).
   (union-r (intersection-r (omega1) (omega2) (omega3))
            (omega4)
            (omega5))))

(define (create-pawn-shape) (omega))
