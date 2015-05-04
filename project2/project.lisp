
(defgeneric display-tensor (tensor) )
(defmethod display-tensor ((tensor number))
	tensor)

(defmethod display-tensor ((tensor vector))
	(loop for index from 0 to (- (length tensor) 1)
		do (format t "~A " (aref tensor index))))


;SCALARS AND VECTORS
(defun s (x)
	x)

(defun v (&rest args)
	(let ((vec (make-array (length args) :initial-contents args :adjustable t)))
		(display-tensor vec)))