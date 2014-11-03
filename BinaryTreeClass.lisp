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

(defparameter *bst-node-type* (make-instance 'binary-tree-node))
(defparameter *bst-leaf-type* (make-instance 'binary-tree-leaf))

(defgeneric bst-tree-leaf-p (value))
(defgeneric bst-tree-node-p (value))
(defgeneric bst-make-empty (search-tree))
(defgeneric bst-empty-p (search-tree))
(defgeneric bst-member-p (search-tree value))
(defgeneric bst-insert (search-tree value))
(defgeneric bst-remove (search-tree value))

(defmethod bst-tree-leaf-p (value)
  "Test if this value is a BST tree leaf."
  (eq (class-of *bst-leaf-type*) value))

(defmethod bst-tree-node-p (value)
  "Test if this value is a BST tree search node."
  (eq (class-of *bst-node-type*) value))

(defmethod bst-make-empty ((search-tree binary-search-tree))
  "Make an empty BST.  If the BST had something in it, GC will delete it!"
  (setf (root-node search-tree) nil))

(defmethod bst-empty-p ((search-tree binary-search-tree))
  "Test whether a tree is empty.  Returns T if tree is empty."
  (= (size search-tree) 0))

(defmethod bst-member-p ((search-tree binary-search-tree) value)
  "A search method.  Returns T if value if BST contains the value."
  (labels (
	   (bst-nonempty-member-p (B E)
	     (if (bst-tree-leaf-p B)
		 (= E (stored-value B))
		 (if (<= E (stored-value B))
		     (bst-nonempty-member-p (left-child B) E)
		     (bst-nonempty-member-p (right-child B) E))))
	   (bst-member-p (B E)
	     (if (bst-empty-p B)
		 nil
		 (bst-nonempty-member-p B E))))
    (bst-member-p search-tree value)))

(defmethod bst-insert ((search-tree binary-search-tree) value)
  nil)

(defmethod bst-remove ((search-tree binary-search-tree) value)
  nil)
