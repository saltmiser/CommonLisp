;; http://stackoverflow.com/questions/9719852/common-lisp-getting-all-keys-of-a-given-hash-table-as-a-list
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

;; A vertex class which contains a localized value (stored-value) 
;; and a hash table which is keyed by other vertices which index
;; to edge values (i.e., actual edge objects)
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
    :initform (error "You must provide a weight value for this to make sense.  Even if all weights are equal.")
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


