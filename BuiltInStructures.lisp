;; Arrays
(defparameter x (make-array 64))
(setf (aref x 0) "A value")

;; Vectors are one-dimensional arrays
(vector 1 2 3 4 5 6) ;; Returns array containing the &rest values

;; Three dimensional arrays
(defparameter WORLD 
  (make-array '(2 2 2) :initial-contents '(((1 2) (3 4)) ((5 6) (7 8)))))
(aref WORLD 0 1 0) ;; Returns 3

;; Hash-tables
(defparameter y (make-hash-table))
(setf (gethash 'somekey y) "Another value")

(time (gethash 'somekey y))
(time (aref x 0))


;; &rest allows you to define a function which accepts infinite parameters:
(defun infinite-add (&rest numbers) 
  (apply #'+ numbers))


