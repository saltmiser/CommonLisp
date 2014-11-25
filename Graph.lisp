;; http://stackoverflow.com/questions/9719852/common-lisp-getting-all-keys-of-a-given-hash-table-as-a-list
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

;; A vertex class which contains a localized value (stored-value) 
;; and a hash table which is keyed by other vertices which index
;; to edge values (i.e., actual edge objects.)
(defclass vertex ()
  ((stored-value
    :initarg :stored-value
    :initform nil
    :accessor stored-value
    :documentation "The item to be stored at this node.")
   (edge-table
    :initarg edge-table 
    :initform (make-hash-table)
    :accessor edge-table
    :documentation "A hash table using vertex objects as keys and edge objects as values.")))

(defclass edge ()
  ;; The verticies connected by this edge are not stored in the edge class.  A vertex structure should keep
  ;; track of edges and where they go.  Use an iedge (informed-edge) if this informaion is important to you.  
  ((weight
    :initarg :weight
    :initform (error "You must provide a comparable value applicable to <, >, =, etc.")
    :accessor weight
    :documentation "The weight of this edge.")))

(defclass iedge (edge)
  ((connected-nodes
    :initarg :connected-nodes
    :initform (error "Please define an empty structure to precedent how you will store connected nodes."))))

;; The graph class contains the hash table which is keyed by verticies
;; and allows access to a list of edge objects.  
(defclass graph ()
  ((vertex-edge-table
    :initform (make-hash-table)
    :accessor vertex-edge-table
    :documentation "Provides an accessor to the vertex/edge (key/value) table.")
   (symbol-vertex-table
    :initform (make-hash-table)
    :accessor symbol-vertex-table
    :documentation "A convenience table to store symbol/vertex (key/symbol) values.")))

;; Follows is a list of generic method definitions which will mutate
;; graph "grph" objects which are made up of vertex "vrtx" and "edge" objects.  
;;
;; Add new vertex to graph with stored-value.
(defgeneric add-vertex (grph vrtx))
(defgeneric add-vertex-by-symbol (grph vrtx-symbol vrtx-stored-value))
;; Add new edge from first-vrtx to second-vrtx with edge-weight.
(defgeneric connect (grph first-vrtx second-vrtx edge-weight))
(defgeneric connect-by-symbols (grph 1-vrtx 2-vrtx edge-weight))
;; Return a list of all neighbors from a vertex.
(defgeneric vertex-neighbors (grph vrtx))
(defgeneric vertex-neighbors-by-symbol (grph symbol)) ;; Convenience methods.
(defgeneric vertex-neighbors-by-symbol-return-symbols (grph vrtx))
;; Traverse graph using shortest path and apply a function along the way.
;; Returns the path length. (Side-effect ridden!).
(defgeneric graph-search-apply-fn (grph start-vrtx end-vrtx function)) 
;; Traverse graph using shortest path and collect result of function
;; along the way.  Returns said collected list. (Very functional!)
(defgeneric graph-search-collect-fn (grph start-vrtx end-vrtx function))
;; Return the vertex count in a graph.
(defgeneric graph-vertex-size (grph))
;; Return the edge count in a graph.
(defgeneric graph-edge-size (grph))
;; Get a vertex with its assigned symbol.
(defgeneric get-vertex-by-symbol (grph symbol))

(defmethod get-vertex-by-symbol ((grph graph) symbol)
  (gethash symbol (symbol-vertex-table grph)))

(defmethod graph-vertex-size ((grph graph))
  (length (hash-keys (vertex-edge-table grph))))

(defmethod graph-edge-size ((grph graph))
  (labels ((edge-counter (grph keylist)
	     (if (= (length keylist) 0)
		 0
		 (+ (length (vertex-neighbors-by-symbol grph (car keylist))) 
		    (edge-counter grph (cdr keylist))))))
    (edge-counter grph (hash-keys (symbol-vertex-table grph)))))

;; Implementation using the graph, vertex and edge class.
(defmethod add-vertex ((grph graph) (vrtx vertex))
  (let ((v-map (vertex-edge-table grph)))
    (setf (gethash vrtx v-map) '()))) ;; Install an empty list for the vertex object.

;; Implementation using the graph, vertex and edge class.
(defmethod add-vertex-by-symbol ((grph graph) vrtx-symbol vrtx-stored-value)
  (let ((ve-map (symbol-vertex-table grph)))
	(setf (gethash vrtx-symbol ve-map)
	  (make-instance 'vertex 
			 :stored-value vrtx-stored-value))
	(add-vertex grph (gethash vrtx-symbol ve-map))))
