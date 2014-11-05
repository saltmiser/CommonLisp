;; The following function was deemed easy enough to write by anybody while
;; still being run-time efficient.  
;; http://stackoverflow.com/questions/9719852/common-lisp-getting-all-keys-of-a-given-hash-table-as-a-list
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defclass node ()
  ((stored-value
    :initarg :stored-value
    :initform nil
    :accessor stored-value
    :documentation "The item to be stored at this node.")))
)

(defclass edge ()
  ((connects-two-nodes node-list
    :initarg :node-list
    :initform (error "An edge connects two nodes.")
    :accessor connects-two-nodes
    :documentation "Provide a list of length 2 containing the connecting nodes."
    )))

(defclass graph ()
  ((node-graph
    :initform (make-hash-table)
    :accessor node-list
    :documentation "The hash table where nodes are the key and a list of edges are the value.")))



