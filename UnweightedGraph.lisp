
(defclass node ()
  ((stored-value
    :initarg :stored-value
    :initform nil
    :accessor stored-value
    :documentation "The item to be stored at this node.")))
)
(defclass edge ()
  ((node-list
    :initarg :node-list
    :initform (error "An edge connects two nodes.")
    :accessor node-list
    :documentation "Provide a list of length 2 containing the connecting nodes."
    )))


