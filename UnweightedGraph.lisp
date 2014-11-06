;; The following function was deemed easy enough to write by anybody while
;; still being run-time efficient.  
;; http://stackoverflow.com/questions/9719852/common-lisp-getting-all-keys-of-a-given-hash-table-as-a-list
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defclass vertex ()
  ((stored-value
    :initarg :stored-value
    :initform nil
    :accessor stored-value
    :documentation "The item to be stored at this node.")
   (distance-value
    :initarg :distance-value
    :initform nil
    :accessor distance-value
    :documentation "Searching for shortest paths will require a distance attribute.  There is no need to create dedicated graphs for searching.")))

(defclass unweighted-edge ()
  ((connects-two-nodes 
    :initarg :node-list
    :initform (error "An edge connects two nodes.")
    :accessor connects-two-nodes
    :documentation "Provide a list of length 2 containing the connecting nodes."
    )))

(defclass graph ()
  ((vertex-edge-map
    :initform (make-hash-table)
    :accessor vertex-edge-map
    :documentation "The hash table where vertex objects are the key and a list of edges are the value.")
   (vertex-map
    :initform (make-hash-table)
    :accessor vertex-map
    :documentation "The hash table where vertex values are mapped to each vertex, this is provided as a convienience for using vertex-edge-map.")))

(defgeneric add-vertex (g vertex-value))
(defgeneric connect-nodes (g first-vertex second-vertex))
(defgeneric traverse-graph (g start-vertex end-vertex function))

(defmethod add-vertex ((g graph) vertex-value)
  (let ((v-map (vertex-map g))
	(ve-map (vertex-edge-map g)))
    (setf (gethash vertex-value v-map)
	  (make-instance 'vertex :stored-value vertex-value))
    (setf (gethash (gethash vertex-value v-map) ve-map)
	  (list))))

(defmethod connect-nodes 
    ((g graph) (first-vertex vertex) (second-vertex vertex))
  (let ((v1 first-vertex)
	(v2 second-vertex)
	(edge-map (vertex-edge-map g)))
    (let ((newedge 
	   (make-instance 'unweighted-edge
			  :node-list (list v1 v2)))
	  (v1-edges (gethash v1 edge-map))
	  (v2-edges (gethash v2 edge-map)))
      (progn 
	(if (and v1-edges (find v1 (connects-two-nodes v1-edges)))
	    nil
	    (setf v1-edges (append v1-edges (list newedge))))
	(if (and v2-edges (find v2 (connects-two-nodes v2-edges)))
	    nil
	    (setf v2-edges (append v2-edges (list newedge))))
	nil))))


(setf myg (make-instance 'graph))
(add-vertex myg 'HELLO)
(add-vertex myg 'GOODBYE)
(connect-nodes myg (gethash 'HELLO (vertex-map myg))
	       (gethash 'GOODBYE (vertex-map myg)))


