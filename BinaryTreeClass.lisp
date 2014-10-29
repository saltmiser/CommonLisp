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
    :documentation "The root BST node for this BST.")))

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
    (inner-bst-insert value rootnode))))


	     
