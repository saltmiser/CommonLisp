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
(defgeneric connect-nodes-by-key (g first-vertex-key second-vertex-key))
(defgeneric get-all-neighbors (g fromvertex))
(defgeneric get-all-neighbors-by-key (g fromvertex-key))
(defgeneric traverse-graph (g start-vertex end-vertex function))
(defgeneric traverse-graph-by-key (g start-vertex-key end-vertex-key function))

(defmethod add-vertex ((g graph) vertex-value)
  (let ((v-map (vertex-map g))
	(ve-map (vertex-edge-map g)))
    (setf (gethash vertex-value v-map)
	  (make-instance 'vertex :stored-value vertex-value))
    (setf (gethash (gethash vertex-value v-map) ve-map)
	  (list))))

(defun object-in-sublist (object list)
  "A helper function to check if an object is in one of the lists within a list (aka, 2d list item detector)."
  (let ((found nil))
    (loop for e in list do
	 (if (find object list) (setf found t)))
    found))

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
	(if (object-in-sublist v1 v1-edges) 
	    nil
	    (setf v1-edges (append v1-edges (list newedge))))
	(if (object-in-sublist v2 v2-edges) 
	    nil
	    (setf v2-edges (append v2-edges (list newedge))))
	(setf (gethash v1 edge-map) v1-edges)
	(setf (gethash v2 edge-map) v2-edges)
	(setf (vertex-edge-map g) edge-map)))))

(defmethod connect-nodes-by-key ((g graph) first-vertex-key second-vertex-key)
  (connect-nodes g 
		 (gethash first-vertex-key (vertex-map g))
		 (gethash second-vertex-key (vertex-map g))))

(trace connect-nodes-by-key connect-nodes vertex-edge-map vertex-edge connects-two-nodes hash-keys)

(defmethod get-all-neighbors ((g graph) (fromvertex vertex))
  (let ((result (list))
	(neighbors (gethash fromvertex (vertex-edge-map g))))
    (loop for e in neighbors do
	 (loop for v in (connects-two-nodes e) do
	      (if (not (eq v fromvertex))
		  (setf result (append result (list v))))))
    result))

(defmethod get-all-neighbors-by-key ((g graph) fromvertex-key)
  (get-all-neighbors g (gethash fromvertex-key (vertex-map g))))

(setf myg (make-instance 'graph))
(add-vertex myg 'HELLO)
(add-vertex myg 'GOODBYE)
(add-vertex myg 'FLOWERS)
(add-vertex myg 'SMELL)
(Add-vertex myg 'FOOD)
(connect-nodes-by-key myg 'HELLO 'GOODBYE)
(connect-nodes-by-key myg 'FLOWERS 'SMELL)
(connect-nodes-by-key myg 'FOOD 'SMELL)



(princ (gethash (gethash 'HELLO (vertex-map myg)) (vertex-edge-map myg)))
(princ (connects-two-nodes (car (gethash (gethash 'HELLO (vertex-map myg)) (vertex-edge-map myg)))))
