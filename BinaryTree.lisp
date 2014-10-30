;; Binary Trees
;; Source: http://www.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/3/tutorial3.html
;;

;; Binary Tree Constructors
(defun make-bin-tree-leaf (E)
  "Create a leaf to store element E."
  (list E))

(defun make-bin-tree-node (E B1 B2)
  "Create a node with element E, left subtree B1 and right subtree B2."
  (list E B1 B2))

;; Selectors for binary trees 
;; (i.e., to pick elements from the tree)
(defun bin-tree-leaf-element (L)
  "Retrieve the element of a leaf L."
  (first L))

(defun bin-tree-node-element (N)
  "Retrieve the element of a node N."
  (first N))

(defun bin-tree-node-left (N)
  "Retrieve the left subtree of a node N."
  (second N))

(defun bin-tree-node-right (N)
  "Retrieve the right subtree of a node N."
  (third N))

;; Recognizers for binary trees
(defun bin-tree-leaf-p (B)
  "Test if binary tree B is a leaf."
  (and (listp B) (= (list-length B) 1)))

(defun bin-tree-node-p (B)
  "Test if binary tree B is a node."
  (and (listp B) (= (list-length B) 3)))

;; (make-bin-tree-node
;;	  '*
;;	  (make-bin-tree-node '+
;;			      (make-bin-tree-leaf 2)
;;			      (make-bin-tree-leaf 3))
;;	  (make-bin-tree-node '-
;;			      (make-bin-tree-leaf 7)
;;			      (make-bin-tree-leaf 8)))
;;

;;(defun bin-tree-member-p (B E)
;;  "Test if E is an element in binary tree B."
;;  (if (bin-tree-leaf-p B)
;;      (equal E (bin-tree-leaf-element B))
;;      (or (equal E (bin-tree-node-element B))
;;	  (bin-tree-member-p (bin-tree-node-left B) E)
;;	  (bin-tree-member-p (bin-tree-node-right B) E))))

;; Below is a slightly-more readable version when using (trace)

(defun bin-tree-member-p (B E)
  "Test if E is an element in binary tree B."
  (if (bin-tree-leaf-p B)
      (equal E (bin-tree-leaf-element B))
    (let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
      (or (equal E elmt)
	  (bin-tree-member-p left E)
	  (bin-tree-member-p right E)))))

;; (trace bin-tree-member-p)
;; (bin-tree-member-p '(+ (* (2) (3)) (- (7) (8))) 7)

;; implement a (bin-tree-size B) function


;; 
;; Traversals through the binary tree 
;;

(defun bin-tree-preorder (B)
  "Create a list containing keys of B in preorder."
  (if (bin-tree-leaf-p B)
      (list (bin-tree-leaf-element B))
      (let
	  ((elmt   (bin-tree-node-element B))
	   (left   (bin-tree-node-left    B))
	   (right  (bin-tree-node-right   B)))
	(cons elmt
	      (append (bin-tree-preorder left)
		      (bin-tree-preorder right))))))

;; Trace the above function and notice the inefficiency

(defun fast-bin-tree-preorder (B)
  "A tail-recursive version of bin-tree-preorder."
  (preorder-aux B nil))

(defun preorder-aux (B A)
  "Append A to the end of the list containing elements of B  in preorder."
  (if (bin-tree-leaf-p B)
      (cons (bin-tree-leaf-element B) A)
      (let
	  ((elmt (bin-tree-node-element B))
	   (left (bin-tree-node-left B))
	   (right (bin-tree-node-right B)))
	(cons elmt
	      (preorder-aux left
			    (preorder-aux right A))))))

;; Trace the above function and notice why it is faster!

;; Implement the traversal for inorder and postorder

;; 
;; Abstract Data Type
;;

;; Here comes the actual binary tree front-end interface!
(defun make-empty-bst ()
  "Create an empty BST."
  nil)

(defun bst-empty-p (B)
  "Check if BST B is empty."
  (null B))

(defun bst-member-p (B E)
  "Check if E is a member of BST B."
  (if (bst-empty-p B)
      nil
      (bst-nonempty-member-p B E)))

(defun bst-nonempty-member-p (B E)
  "Check if E is a member of nonempty BST B."
  (if (bin-tree-leaf-p B)
      (= E (bin-tree-leaf-element B))
      (if (<= E (bin-tree-node-element B))
	  (bst-nonempty-member-p (bin-tree-node-left B) E)
	  (bst-nonempty-member-p (bin-tree-node-right B) E))))

(defun bst-insert (B E)
  "Insert E into BST B."
  (if (bst-empty-p B)
      (make-bin-tree-leaf E)
      (bst-nonempty-insert B E)))

(defun bst-nonempty-insert (B E)
  "Insert E into nonempty BST B."
  (if (bin-tree-leaf-p B)
      (bst-leaf-insert B E)
      (let ((elmt (bin-tree-node-element B))
	    (left (bin-tree-node-left B))
	    (right (bin-tree-node-right B)))
	(if (<= E (bin-tree-node-element B))
	    (make-bin-tree-node elmt
				(bst-nonempty-insert (bin-tree-node-left B) E)
				right)
	    (make-bin-tree-node elmt
				left
				(bst-nonempty-insert (bin-tree-node-right B) 
						     E))))))

(defun bst-leaf-insert (L E)
  "Insert element E to a BST with only one leaf."
  (let ((elmt (bin-tree-leaf-element L)))
    (if (= E elmt)
	L
	(if (< E elmt)
	    (make-bin-tree-node E
				(make-bin-tree-leaf E)
				(make-bin-tree-leaf elmt))
	    (make-bin-tree-node elmt
				(make-bin-tree-leaf elmt)
				(make-bin-tree-leaf E))))))
 

  
;; (trace bst-insert bst-nonempty-insert bst-leaf-insert)
;; (bst-insert '(2 (1 (1) (2)) (3 (3) (4))) 2.5)

(defun bst-remove (B E)
  "Remove E from BST B."
  (if (bst-empty-p B)
      B
      (if (bin-tree-leaf-p B)
	  (bst-leaf-remove B E)
	  (bst-node-remove B E))))

(defun bst-leaf-remove (L E)
  "Remove E from BST leaf L."
  (if (= E (bin-tree-leaf-element L))
      (make-empty-bst)
      L))

(defun bst-node-remove (N E)
  "Remove E from BST node N."
  (let 
      ((elmt (bin-tree-node-element N))
       (left (bin-tree-node-left N))
       (right (bin-tree-node-right N)))
    (if (<= E elmt)
	(if (bin-tree-leaf-p left)
	    (if (= E (bin-tree-leaf-element left))
		right
		N)
	    (make-bin-tree-node elmt (bst-node-remove left E) right))
	(if (bin-tree-leaf-p right)
	    (if (= E (bin-tree-leaf-element right))
		left
		N)
	    (make-bin-tree-node elmt left (bst-node-remove right E))))))

;; (trace bst-remove bst-node-remove)
;; (bst-remove '(2 (1 (1) (2)) (3 (3) (4))) 3)

;; Implement a set ADT which orders set items into the above bst ADT.  

