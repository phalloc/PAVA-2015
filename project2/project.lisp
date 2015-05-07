(defclass tensor()
  ((value :accessor tensor-value :initarg :value)))

(defclass scalar(tensor)
  ((value :accessor scalar-value :initarg :value)))
  
(defclass vec(tensor)
  ((value :accessor vec-value :initarg :value)))
 
 
(defmethod print-object ((tens tensor) stream)
  (print-object (tensor-value tens) stream))
 
(defmethod print-object ((v array) stream)
  (loop for x from 0 below (array-dimension v 0) do
    (format stream "~A " (aref v x)))
    (format stream "~%"))
    

		
;SCALARS AND VECTORS
(defun s (x)
	(make-instance 'scalar :value x))

(defun v (&rest values)
  (let ((result (make-instance 'vec :value (make-array (list-length values)))))
    (loop for index from 0 below (list-length values)
       do (setf (aref (vec-value result) index) (s (nth index values))))
    result))
 

		
;MONADIC FUNCTIONS


; Symmetric (.-)
(defgeneric .- (tensor) )

  
(defmethod .- ((tensor scalar))
  (s (* -1 (scalar-value tensor))))


(defmethod .- ((tensor vec))
  (execute-monadic-fun (vec-value tensor) #'.-))
    
; Inverse (./)

(defgeneric ./ (tensor) )

(defmethod ./ ((tensor scalar))
  (s (/ 1 (scalar-value tensor))))

(defmethod ./ ((tensor vec))
  (execute-monadic-fun (vec-value tensor) #'./))
		
; Factorial (.!)

(defun fact (n)
  (if (eql n 0) 
      1
      (* n (fact (- n 1)))))

(defgeneric .! (tensor) )

(defmethod .! ((tensor scalar))
  (s (fact (scalar-value tensor))))

(defmethod .! ((tensor vec))
  (execute-monadic-fun (vec-value tensor) #'.!))

; sin (.sin)

(defgeneric .sin (tensor) )

(defmethod .sin ((tensor scalar))
  (s (sin (scalar-value tensor))))

(defmethod .sin ((tensor vec))
  (execute-monadic-fun (vec-value tensor) #'.sin))

; cos (.cos)

(defgeneric .cos (tensor) )

(defmethod .cos ((tensor scalar))
  (s (cos (scalar-value tensor))))

(defmethod .cos ((tensor vec))
  (execute-monadic-fun (vec-value tensor) #'.cos))

; not (.not)

(defgeneric .not (tensor) )

(defmethod .not ((tensor scalar))
  (if (eql (scalar-value tensor) 0)
      1
      0))

(defmethod .not ((tensor vec))
  (execute-monadic-fun (vec-value tensor) #'.not))

; Reshape


; Shape

(defgeneric shape (tensor))

(defmethod shape ((tensor scalar))
  0)

(defmethod shape ((tensor vec))
  (length (vec-value tensor)))

; Interval

(defun interval (n)
  (let ((lst nil))
    (loop for index from 0 below n
       do (setf lst (cons (+ index 1) lst)))
    (make-instance 'vec :value (make-array (list-length lst) :initial-contents (reverse lst)))))

; DYADIC FUNCTIONS


; Sum (.+)

(defgeneric .+ (tensor1 tensor2))

(defmethod .+ ((tensor1 scalar) (tensor2 scalar))
  (s (+ (scalar-value tensor1) (scalar-value tensor2))))

(defmethod .+ ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.+))


(defmethod .+ ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.+))

(defmethod .+ ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.+)
      (print "Error: Tensors have different sizes")))


; Sub (.-)

; TODO

; Mul (.*)

(defgeneric .* (tensor1 tensor2))

(defmethod .* ((tensor1 scalar) (tensor2 scalar))
  (s (* (scalar-value tensor1) (scalar-value tensor2))))

(defmethod .* ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.*))

(defmethod .* ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.*))

(defmethod .* ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.*)
      (print "Error: Tensors have different sizes")))

; Div (./)

; TODO


; Integer Division (.//)

(defgeneric .// (tensor1 tensor2))

(defmethod .// ((tensor1 scalar) (tensor2 scalar))
  (let ((result (floor (scalar-value tensor1) (scalar-value tensor2))))
  (s result)))

(defmethod .// ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.//))

(defmethod .// ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.//))

(defmethod .// ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.//)
      (print "Error: Tensors have different sizes")))

; Remainder (.%)

(defgeneric .% (tensor1 tensor2))

(defmethod .% ((tensor1 scalar) (tensor2 scalar))
  (s (mod (scalar-value tensor1) (scalar-value tensor2))))

(defmethod .% ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.%))

(defmethod .% ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.%))

(defmethod .% ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.%)
      (print "Error: Tensors have different sizes")))

; Lesser than (.<)

(defgeneric .< (tensor1 tensor2))

(defmethod .< ((tensor1 scalar) (tensor2 scalar))
  (if (< (scalar-value tensor1) (scalar-value tensor2))
      1
      0))

(defmethod .< ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.<))

(defmethod .< ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.<))

(defmethod .< ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.<)
      (print "Error: Tensors have different sizes")))

; Greater than (.>)

(defgeneric .> (tensor1 tensor2))

(defmethod .> ((tensor1 scalar) (tensor2 scalar))
  (if (> (scalar-value tensor1) (scalar-value tensor2))
      1
      0))

(defmethod .> ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.>))

(defmethod .> ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.>))

(defmethod .> ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.>)
      (print "Error: Tensors have different sizes")))

; Lesser or Equal than (.<=)

(defgeneric .<= (tensor1 tensor2))

(defmethod .<= ((tensor1 scalar) (tensor2 scalar))
  (if (<= (scalar-value tensor1) (scalar-value tensor2))
      1
      0))

(defmethod .<= ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.<=))

(defmethod .<= ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.<=))

(defmethod .<= ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.<=)
      (print "Error: Tensors have different sizes")))

; Greater or Equal than (.<=)

(defgeneric .>= (tensor1 tensor2))

(defmethod .>= ((tensor1 scalar) (tensor2 scalar))
  (if (>= (scalar-value tensor1) (scalar-value tensor2))
      1
      0))

(defmethod .>= ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.>=))

(defmethod .>= ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.>=))

(defmethod .>= ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.>=)
      (print "Error: Tensors have different sizes")))

; Equal (.=)

(defgeneric .= (tensor1 tensor2))

(defmethod .= ((tensor1 scalar) (tensor2 scalar))
   (if (= (scalar-value tensor1) (scalar-value tensor2))
      1
      0))

(defmethod .= ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.=))

(defmethod .= ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.=))

(defmethod .= ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.=)
      (print "Error: Tensors have different sizes")))

; Or (.or)

(defgeneric .or (tensor1 tensor2))

(defmethod .or ((tensor1 scalar) (tensor2 scalar))
  (if (or tensor1 tensor2)
      1
      0))

(defmethod .or ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.or))

(defmethod .or ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.or))

(defmethod .or ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.or)
      (print "Error: Tensors have different sizes")))


; And (.and)

(defgeneric .and (tensor1 tensor2))

(defmethod .and ((tensor1 scalar) (tensor2 scalar))
  (if (= (scalar-value tensor1) (scalar-value tensor2))
      1
      0))

(defmethod .and ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun2 (vec-value tensor2) tensor1 #'.and))

(defmethod .and ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun2 (vec-value tensor1) tensor2 #'.and))

(defmethod .and ((tensor1 vec) (tensor2 vec))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (execute-dyadic-fun (vec-value tensor1) (vec-value tensor2) #'.and)
      (print "Error: Tensors have different sizes")))

; Drop (drop)



; Reshape (reshape)

; Catenate (catenate)

(defgeneric catenate (tensor1 tensor2))

(defmethod catenate ((tensor1 scalar) (tensor2 scalar))
  (vector tensor1 tensor2))

; PARTIDA
(defmethod catenate ((tensor1 vector) (tensor2 vector))
  (let ((result (make-array (+ (length tensor1) (length tensor2)) :fill-pointer 0))) 
    (loop for index from 0 to (- (length tensor1) 1)
	 do (vector-push (aref tensor1 index) result))
    (loop for index from 0 to (- (length tensor2) 1)
	 do (vector-push (aref tensor2 index) result))
    result))

; Member (member)

(defgeneric member? (tensor1 tensor2))

; NEEDS TO BE CORRECTED
;(defmethod member? ((tensor1 vec) (tensor2 scalar))
;  (let ((result (make-array (length (vec-value tensor1)))))
;    (loop for index from 0 below (array-dimension (vec-value tensor1) 0)
;       if (eql (aref (vec-value tensor1) index) tensor2)
;       do (setf (aref result index) 1)
;       else 
;       do (setf (aref result index) 0))
;    result))


    
; Auxiliary Functions

(defun execute-monadic-fun (vec fun)
  (let ((lst nil))
    (loop for index from 0 below (array-dimension vec 0)
       do (setf lst (cons (funcall fun (aref vec index)) lst)))
    (make-instance 'vec :value (make-array (list-length lst) :initial-contents (reverse lst)))))

(defun execute-dyadic-fun (vec1 vec2 fun)
  (let ((lst nil))
    (loop for index from 0 below (array-dimension vec1 0)
       do (setf lst (cons	(funcall fun (aref vec1 index)
			     (aref vec2 index)) 
		lst)))
    (make-instance 'vec :value (make-array (list-length lst) :initial-contents (reverse lst)))))

(defun execute-dyadic-fun2 (vec sca fun)
  (let ((lst nil))
    (loop for index from 0 below (array-dimension vec 0)
	 do (setf lst (cons (funcall fun sca
			       (aref vec index))
		  lst)))
    (make-instance 'vec :value (make-array (list-length lst) :initial-contents (reverse lst)))))
