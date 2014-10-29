;;
;; Polynomials
;; 

;; Constructors for polynomials
(defun make-constant (num)
  num)

(defun make-variable (sym)
  sym)

(defun make-sum (poly1 poly2)
  (list '+ poly1 poly2))

(defun make-product (poly1 poly2)
  (list '* poly1 poly2))

(defun make-power (poly num)
  (list '** poly num))

;; For example,
;; (make-power (make-variable 'x) (make-constant 1)) 2) becomes (** (+ x 1) 2)
;; which becomes (x+1)^2, a typical polynomial.

;; Recognizers for polynomials

(defun constant-p (poly)
  (numberp poly))

(defun variable-p (poly)
  (symbolp poly))

(defun sum-p (poly)
  (and (listp poly) (eq (first poly) '+)))

(defun product-p (poly)
  (and (listp poly) (eq (first poly) '*)))

(defun power-p (poly)
  (and (listp poly) (eq (first poly) '**)))

;; Selectors for polynomials
(defun constant-numeric (const)
  const)

(defun variable-symbol (var)
  var)

(defun sum-arg1 (sum)
  (second sum))

(defun sum-arg2 (sum)
  (third sum))

(defun product-arg1 (prod)
  (second prod))

(defun product-arg2 (prod)
  (third prod))

(defun power-base (pow)
  (second pow))

(defun power-exponent (pow)
  (third pow))

;; The above code is simply an interface to help make this code 
;; self-documenting

;; The following code pertains to solving derivatives of our expressions
(defun make-derivative (poly x)
  (list 'd poly x))

(defun derivative-p (poly x)
  (and (listp poly) (eq (first poly) 'd)))

(defun d (poly x)
  (cond 
    ((constant-p poly) 0)
    ((variable-p poly)
     (if (equal poly x)
	 1
	 (make-derivative poly x)))
    ((sum-p poly)
     (make-sum (d (sum-arg1 poly) x)
	       (d (sum-arg2 poly) x)))
    ((product-p poly)
     (make-sum (make-product (product-arg1 poly)
			     (d (product-arg2 poly) x))
	       (make-product (product-arg2 poly)
			     (d (product-arg1 poly) x))))
    ((power-p poly)
     (make-product (make-product (power-exponent poly)
				 (make-power (power-base poly)
					     (1- (power-exponent poly))))
		   (d (power-base poly) x)))))

;; (d '(+ x y) 'x)
;; (d '(* (+ x 1) (+ x 1)) 'x)


;; Simplification function for a polynomial (and their rules)
(defun simplify (poly)
  "Simplify polynomial POLY."
  (cond
   ((constant-p poly) poly)
   ((variable-p poly) poly)
   ((sum-p poly)
    (let ((arg1 (simplify (sum-arg1 poly)))
	  (arg2 (simplify (sum-arg2 poly))))
      (make-simplified-sum arg1 arg2)))
   ((product-p poly)
    (let ((arg1 (simplify (product-arg1 poly)))
	  (arg2 (simplify (product-arg2 poly))))
      (make-simplified-product arg1 arg2)))
   ((power-p poly)
    (let ((base (simplify (power-base poly)))
	  (exponent (simplify (power-exponent poly))))
      (make-simplified-power base exponent)))
   ((derivative-p poly) poly)))

(defun make-simplified-sum (arg1 arg2)
  "Given simplified polynomials ARG1 and ARG2, construct a simplified sum of ARG1 and ARG2."
  (cond
   ((and (constant-p arg1) (zerop arg1)) arg2)
   ((and (constant-p arg2) (zerop arg2)) arg1)
   (t                                    (make-sum arg1 arg2))))

(defun make-simplified-product (arg1 arg2)
  "Given simplified polynomials ARG1 and ARG2, construct a simplified product of ARG1 and ARG2."
  (cond
   ((and (constant-p arg1) (zerop arg1)) (make-constant 0))
   ((and (constant-p arg2) (zerop arg2)) (make-constant 0))
   ((and (constant-p arg1) (= arg1 1))   arg2)
   ((and (constant-p arg2) (= arg2 1))   arg1)
   (t                                    (make-product arg1 arg2))))

(defun make-simplified-power (base exponent)
  "Given simplified polynomials BASE and EXPONENT, construct a simplified power with base BASE and exponent EXPONENT."
  (cond
   ((and (constant-p exponent) (= exponent 1))   base)
   ((and (constant-p exponent) (zerop exponent)) (make-constant 1))
   (t                          (make-power base exponent))))

;; (simplify (d '(* (+ x 1) (+ x 1)) 'x))
;; (simplify (d '(** (+ x 1) 2) 'x))



