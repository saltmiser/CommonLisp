(defparameter *account-numbers* 0)
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name.")
    :accessor customer-name
    :documentation "Customer's Name")
   (balance
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, or :bronze.")))

(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account'balance)))
    (setf (slot-value account 'account-type)
	  (cond
	    ((>= balance 100000) :gold)
	    ((>= balance 50000) :silver)
	    (t :bronze)))))
(defmethod initialize-instance :after ((account bank-account) 
				       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
	  (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

(defgeneric balance (account))
(defmethod balance ((account bank-account))
  (slot-value account 'balance))
(defgeneric (setf customer-name) (value account))
(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name ) value))
(defgeneric customer-name (account))
(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))

(defun make-bank-account-with-balance-and-bonus-percentage
    (holder-name initial-balance bonus-percent)
  (make-instance 'bank-account
	:customer-name holder-name
	:balance initial-balance
	:opening-bonus-percentage bonus-percent))
(defun make-bank-account-with-balance (holder-name initial-balance)
  (make-instance 'bank-account 
		 :customer-name holder-name
		 :balance initial-balance))
(defun make-bank-account (holder-name)
  (make-instance 'bank-account
		 :customer-name holder-name))
(defvar *account-objects* nil)
(push (make-bank-account-with-balance "Jim C." 98887.22) *account-objects*)
(push (make-bank-account-with-balance-and-bonus-percentage
       "Hanna T." 32221.98 3.2) *account-objects*)
