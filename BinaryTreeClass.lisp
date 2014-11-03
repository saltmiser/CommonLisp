(defclass binary-tree-leaf ()
  ((stored-value
    :initarg :stored-value
    :initform nil
    :accessor stored-value
    :documentation "A final value which is actually stored in the BST.")
   (lazy-deleted
    :initarg :lazy-deleted
    :initform nil
    :accessor lazy-deleted
    :documentation "If using lazy deletion, T if this leaf is lazy deleted")))

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
    :documentation "The size of the BST.")
   (lazy-deletion
    :initarg :lazy-deletion
    :initform (error "Specify nil or T for lazy-deletion parameter.")
    :reader lazy-deletion ;; public reader since this may not be changed
    :documentation "Does this BST use lazy deletion?")))

(defgeneric bst-make-empty (search-tree))
(defgeneric bst-empty-p (search-tree))
(defgeneric bst-member-p (search-tree value))
(defgeneric bst-insert (search-tree value))
(defgeneric bst-remove (search-tree value))

(defmethod bst-make-empty ((search-tree binary-search-tree))
  (setf (root-node search-tree) nil))

(defmethod bst-empty-p ((search-tree binary-search-tree))
  nil)

(defmethod bst-member-p ((search-tree binary-search-tree) value)
  nil)

(defmethod bst-insert ((search-tree binary-search-tree) value)
  nil)

(defmethod bst-remove ((search-tree binary-search-tree) value)
  nil)
