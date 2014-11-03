(defclass binary-tree-leaf ()
  ((stored-value
    :initarg :stored-value
    :initform nil
    :accessor stored-value
    :documentation "A final value which is actually stored in the BST.")))

(defclass binary-tree-node (binary-tree-leaf)
  ((left-child
    :initarg :left-child
    :initform nil
    :accessor left-child
    :documentation "The left sub-tree of this BST node.")
   (right-child
    :initarg :right-child
    :initform nil
    :accessor right-child
    :documentation "The right sub-tree of this BST node.")))

(defclass binary-search-tree ()
  ((root-node
    :initarg :root-node
    :initform (make-instance 'binary-tree-node)
    :accessor root-node
    :documentation "The root BST node for this BST.")
   (size ;; Size is a "private static" integer, and insert/remove adjusts this
    :allocation :class 
    :initform 0
    :reader size ;; A public reader is provided, however!
    :documentation "The size of the BST.")))

(defgeneric bst-contains (value search-tree))
(defmethod bst-contains (value (search-tree binary-search-tree))
  (let ((rootnode (root-node search-tree)))
    (labels ((inner-bst-contains (value search-tree)
	       (if (eq (stored-value search-tree) nil)
		   nil
		   (if (eq (stored-value search-tree) value)
		       t
		       (if (< value (stored-value search-tree))
			   (if (eq (left-child search-tree) nil)
			       nil
			       (inner-bst-contains
				value (left-child search-tree)))
			   
			   (if (eq (right-child search-tree) nil)
			       nil
			       (inner-bst-contains 
				value (right-child search-tree)))
			   )))))
      (inner-bst-contains value rootnode))))

(defgeneric bst-insert (value search-tree))
(defmethod bst-insert (value (search-tree binary-search-tree))
  (let ((rootnode (root-node search-tree)))
    (labels ((inner-bst-insert (value search-tree)
	       (if (eq (stored-value search-tree) nil)
		   (setf (stored-value search-tree) value)
		   (if (< value (stored-value search-tree))
		       (if (eq (left-child search-tree) nil)
			   (setf (left-child search-tree) 
				 (make-instance 'binary-tree-node
						:stored-value value))
			   (inner-bst-insert value (left-child search-tree)))
		       (if (eq (right-child search-tree) nil)
			   (setf (right-child search-tree) 
				 (make-instance 'binary-tree-node
						:stored-value value))
			   (inner-bst-insert value (right-child search-tree))))
		   )))
      (inner-bst-insert value rootnode)))
  (setf (slot-value search-tree 'size) (+ 1 (slot-value search-tree 'size))))


