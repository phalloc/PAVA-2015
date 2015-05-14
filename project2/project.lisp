;;;;;;;;;;;;;;;;;;;;;;;; GROUP 7 ;;;;;;;;;;;;;;;;;;;;;;;;
; Diogo Costa - nº 72770                                ;
; Joana Teixeira - nº 73393                             ;
; Tiago Diogo - nº 73559                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;				       


(defclass tensor()
  ((value :accessor tensor-value :initarg :value)))

(defclass scalar(tensor)
  ((value :accessor scalar-value :initarg :value)))
  
(defclass vec(tensor)
  ((value :accessor vec-value :initarg :value)))

;matrix-value: (make-array (lines.nr line.dim))
(defclass matrix(tensor)
  ((value :accessor matrix-value :initarg :value)
   (dimensions :accessor matrix-dimensions :initarg :dimensions)))
 
(defmethod print-object ((tens tensor) stream)
  (print-object (tensor-value tens) stream))
 
(defmethod print-object ((tens vec) stream)
  (loop for index from 0 below (length (vec-value tens)) do
    (format stream "~A " (aref (vec-value tens) index))))

(defmethod print-object ((mat matrix) stream)

  (let* ((special-dims (get-special-dims (matrix-dimensions mat)))
         (dims (make-array (list-length special-dims) :initial-contents special-dims))
         (line 0)
         (space-vector (make-array (sum-special-dims special-dims) :initial-element 1))
         (spaces 1)
         (v-index 0))

        (loop for pace from 1 below (list-length special-dims)
          do (progn 
                (loop for index from 0 below pace
                    do (setf spaces (* spaces (aref dims index)))
                  )

                (setf v-index (1- spaces))
                (loop for index from (1- spaces) below (length space-vector)
                  do (progn (incf (aref space-vector v-index))
                             (setf v-index (+ v-index spaces))
                             (if (> v-index (length space-vector))
                              (return)
                              )
                       )
                  )
                (setf spaces 1)
               )
        )
      (setf (aref space-vector (1- (length space-vector))) 0)
      (loop for iteration from 0 below (sum-special-dims special-dims)

          do (progn (print-block mat (first (matrix-dimensions mat)) line stream)
                    (if (< iteration (1- (sum-special-dims special-dims)))
                      (format stream "~%"))
                    (print-spaces (aref space-vector iteration) stream)
		    (setf line (+ line (first (matrix-dimensions mat)))))
       )
  )
)


(defun print-block (mat dims line stream)

  (loop for block from 0 below dims
      do (progn 
            (print-object (aref (matrix-value mat) line) stream)

            (if (not (eql block (1- dims)))
              (format stream "~%"))

            (incf line)
        ))
  )

(defun print-spaces (number stream)
  (loop for i from 0 below number
    do (format stream "~%")
    )
  )
		
;SCALARS AND VECTORS
(defun s (x)
	(make-instance 'scalar :value x))

(defun v (&rest values)
  (let ((result (make-array (list-length values))))
    (loop for index from 0 below (list-length values)
	  do (setf (aref result index) (s (nth index values))))
  (make-instance 'vec :value result)))

		
;MONADIC FUNCTIONS


; Symmetric & Sub (.-)
(defgeneric .- (tensor &rest tensors))

  
(defmethod .- ((tensor scalar) &rest tensors)
  (cond ((null tensors) (s (* -1 (scalar-value tensor))))
        ((eql (type-of (first tensors)) 'SCALAR) (s (- (scalar-value tensor) (scalar-value (first tensors)))))
        ((eql (type-of (first tensors)) 'VEC) (execute-dyadic-fun-SV tensor (vec-value (first tensors)) #'.-))
        ((eql (type-of (first tensors)) 'MATRIX) (execute-dyadic-fun-SM tensor (first tensors) #'.-)))) 


(defmethod .- ((tensor vec) &rest tensors)
  (cond ((null tensors) (execute-monadic-fun-V (vec-value tensor) #'.-))
        ((eql (type-of (first tensors)) 'SCALAR) (execute-dyadic-fun-VS (vec-value tensor) (first tensors) #'.-))
        ((and (equal (type-of (first tensors)) 'VEC)
              (equal-shape? (shape tensor) (shape (first tensors))))
          (execute-dyadic-fun-VV (vec-value tensor) (vec-value (first tensors)) #'.-))
        (T (print "Error: Tensors have different sizes"))))

(defmethod .- ((tensor matrix) &rest tensors)
  (cond ((null tensors) (execute-monadic-fun-M tensor #'.-))
        ((eql (type-of (first tensors)) 'SCALAR) (execute-dyadic-fun-MS tensor (first tensors) #'.-))
        ((and (equal (type-of (first tensors)) 'MATRIX)
              (equal-shape? (shape tensor) (shape (first tensors))))
          (execute-dyadic-fun-MM tensor (first tensors) #'.-))
        (T (print "Error: Tensors have different sizes"))))
    
; Inverse & Div (./)

(defgeneric ./ (tensor &rest tensors) )

(defmethod ./ ((tensor scalar) &rest tensors)
  (cond ((null tensors) (s (/ 1 (scalar-value tensor))))
        ((eql (type-of (first tensors)) 'SCALAR) (s (/ (scalar-value tensor) (scalar-value (first tensors)))))
        ((eql (type-of (first tensors)) 'VEC) (execute-dyadic-fun-SV tensor (vec-value (first tensors)) #'./))
	((eql (type-of (first tensors)) 'MATRIX) (execute-dyadic-fun-SM tensor (first tensors) #'./)))) 


(defmethod ./ ((tensor vec) &rest tensors)
  (cond ((null tensors) (execute-monadic-fun-V (vec-value tensor) #'./))
        ((eql (type-of (first tensors)) 'SCALAR) (execute-dyadic-fun-VS (vec-value tensor) (first tensors) #'./))
        ((and (equal (type-of (first tensors)) 'VEC)
              (equal-shape? (shape tensor) (shape (first tensors))))
          (execute-dyadic-fun-VV (vec-value tensor) (vec-value (first tensors)) #'./))
        (T (print "Error: Tensors have different sizes"))))

(defmethod ./ ((tensor matrix) &rest tensors)
  (cond ((null tensors) (execute-monadic-fun-M tensor #'./))
        ((eql (type-of (first tensors)) 'SCALAR) (execute-dyadic-fun-MS tensor (first tensors) #'./))
        ((and (equal (type-of (first tensors)) 'MATRIX)
              (equal-shape? (shape tensor) (shape (first tensors))))
          (execute-dyadic-fun-MM tensor (first tensors) #'./))
        (T (print "Error: Tensors have different sizes"))))
		
; Factorial (.!)

(defun fact (n)
  (if (eql n 0) 
      1
      (* n (fact (- n 1)))))

(defgeneric .! (tensor) )

(defmethod .! ((tensor scalar))
  (s (fact (scalar-value tensor))))

(defmethod .! ((tensor vec))
  (execute-monadic-fun-V (vec-value tensor) #'.!))

(defmethod .! ((tensor matrix))
  (execute-monadic-fun-M tensor #'.!))

; sin (.sin)

(defgeneric .sin (tensor) )

(defmethod .sin ((tensor scalar))
  (s (sin (scalar-value tensor))))

(defmethod .sin ((tensor vec))
  (execute-monadic-fun-V (vec-value tensor) #'.sin))

(defmethod .sin ((tensor matrix))
  (execute-monadic-fun-M tensor #'.sin))

; cos (.cos)

(defgeneric .cos (tensor) )

(defmethod .cos ((tensor scalar))
  (s (cos (scalar-value tensor))))

(defmethod .cos ((tensor vec))
  (execute-monadic-fun-V (vec-value tensor) #'.cos))

(defmethod .cos ((tensor matrix))
  (execute-monadic-fun-M tensor #'.cos))

; not (.not)

(defgeneric .not (tensor) )

(defmethod .not ((tensor scalar))
  (if (eql (scalar-value tensor) 0)
      (s 1)
      (s 0)))

(defmethod .not ((tensor vec))
  (execute-monadic-fun-V (vec-value tensor) #'.not))

(defmethod .not ((tensor matrix))
  (execute-monadic-fun-M tensor #'.not))


; Shape

(defgeneric shape (tensor))

(defmethod shape ((tensor scalar))
  (v ))

(defmethod shape ((tensor vec))
  (v (length (vec-value tensor))))

(defmethod shape ((tensor matrix))
  (let ((result (make-array (list-length (matrix-dimensions tensor)))))
    (loop for i from 0 below (list-length (matrix-dimensions tensor))
	  do (setf (aref result i) (s (nth i (matrix-dimensions tensor)))))
  (make-instance 'vec :value result)))
   
; Interval

(defun interval (n)
  (let ((result (make-array n)))
    (loop for index from 0 below n
       do (setf (aref result index) (s (+ index 1))))
    (make-instance 'vec :value result)))

; DYADIC FUNCTIONS


; Sum (.+)

(defgeneric .+ (tensor1 tensor2))

(defmethod .+ ((tensor1 scalar) (tensor2 scalar))
  (s (+ (scalar-value tensor1) (scalar-value tensor2))))

(defmethod .+ ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.+))

(defmethod .+ ((tensor1 scalar) (tensor2 matrix))
  (execute-dyadic-fun-SM tensor1 tensor2 #'.+))

(defmethod .+ ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.+))

(defmethod .+ ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.+)
      (print "Error: Tensors have different sizes")))

(defmethod .+ ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.+))

(defmethod .+ ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.+)
    (print "Error: Tensors have different sizes")))


; Mul (.*)

(defgeneric .* (tensor1 tensor2))

(defmethod .* ((tensor1 scalar) (tensor2 scalar))
  (s (* (scalar-value tensor1) (scalar-value tensor2))))

(defmethod .* ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.*))

(defmethod .* ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.*))

(defmethod .* ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.*)
    (print "Error: Tensors have different sizes")))

(defmethod .* ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.*))

(defmethod .* ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.*)
    (print "Error: Tensors have different sizes")))


; Integer Division (.//)

(defgeneric .// (tensor1 tensor2))

(defmethod .// ((tensor1 scalar) (tensor2 scalar))
  (let ((result (floor (scalar-value tensor1) (scalar-value tensor2))))
  (s result)))

(defmethod .// ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.//))

(defmethod .// ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.//))

(defmethod .// ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.//)
    (print "Error: Tensors have different sizes")))

(defmethod .// ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.//))

(defmethod .// ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.//)
    (print "Error: Tensors have different sizes")))

; Remainder (.%)

(defgeneric .% (tensor1 tensor2))

(defmethod .% ((tensor1 scalar) (tensor2 scalar))
  (s (mod (scalar-value tensor1) (scalar-value tensor2))))

(defmethod .% ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.%))

(defmethod .% ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.%))

(defmethod .% ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.%)
    (print "Error: Tensors have different sizes")))

(defmethod .% ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.%))

(defmethod .% ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.%)
    (print "Error: Tensors have different sizes")))

; Lesser than (.<)

(defgeneric .< (tensor1 tensor2))

(defmethod .< ((tensor1 scalar) (tensor2 scalar))
  (if (< (scalar-value tensor1) (scalar-value tensor2))
      (s 1)
      (s 0)))

(defmethod .< ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.<))

(defmethod .< ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.<))

(defmethod .< ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.<)
    (print "Error: Tensors have different sizes")))

(defmethod .< ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.<))

(defmethod .< ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.<)
    (print "Error: Tensors have different sizes")))

; Greater than (.>)

(defgeneric .> (tensor1 tensor2))

(defmethod .> ((tensor1 scalar) (tensor2 scalar))
  (if (> (scalar-value tensor1) (scalar-value tensor2))
      (s 1)
      (s 0)))

(defmethod .> ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.>))

(defmethod .> ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.>))

(defmethod .> ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.>)
    (print "Error: Tensors have different sizes")))

(defmethod .> ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.>))

(defmethod .> ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.>)
    (print "Error: Tensors have different sizes")))

; Lesser or Equal than (.<=)

(defgeneric .<= (tensor1 tensor2))

(defmethod .<= ((tensor1 scalar) (tensor2 scalar))
  (if (<= (scalar-value tensor1) (scalar-value tensor2))
      (s 1)
      (s 0)))

(defmethod .<= ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.<=))

(defmethod .<= ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.<=))

(defmethod .<= ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.<=)
    (print "Error: Tensors have different sizes")))

(defmethod .<= ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.<=))

(defmethod .<= ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.<=)
    (print "Error: Tensors have different sizes")))


; Greater or Equal than (.<=)

(defgeneric .>= (tensor1 tensor2))

(defmethod .>= ((tensor1 scalar) (tensor2 scalar))
  (if (>= (scalar-value tensor1) (scalar-value tensor2))
      (s 1)
      (s 0)))

(defmethod .>= ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.>=))

(defmethod .>= ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.>=))

(defmethod .>= ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.>=)
    (print "Error: Tensors have different sizes")))

(defmethod .>= ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.>=))

(defmethod .>= ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.>=)
    (print "Error: Tensors have different sizes")))

; Equal (.=)

(defgeneric .= (tensor1 tensor2))

(defmethod .= ((tensor1 scalar) (tensor2 scalar))
   (if (= (scalar-value tensor1) (scalar-value tensor2))
      (s 1)
      (s 0)))

(defmethod .= ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.=))

(defmethod .= ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.=))

(defmethod .= ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.=)
    (print "Error: Tensors have different sizes")))

(defmethod .= ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.=))

(defmethod .= ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.=)
    (print "Error: Tensors have different sizes")))

; Or (.or)

(defgeneric .or (tensor1 tensor2))

(defmethod .or ((tensor1 scalar) (tensor2 scalar))
  (if (or tensor1 tensor2)
      (s 1)
      (s 0)))

(defmethod .or ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.or))

(defmethod .or ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.or))

(defmethod .or ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.or)
    (print "Error: Tensors have different sizes")))

(defmethod .or ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.or))

(defmethod .or ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.or)
    (print "Error: Tensors have different sizes")))


; And (.and)

(defgeneric .and (tensor1 tensor2))

(defmethod .and ((tensor1 scalar) (tensor2 scalar))
  (if (= (scalar-value tensor1) (scalar-value tensor2))
      (s 1)
      (s 0)))

(defmethod .and ((tensor1 scalar) (tensor2 vec))
  (execute-dyadic-fun-SV tensor1 (vec-value tensor2) #'.and))

(defmethod .and ((tensor1 vec) (tensor2 scalar))
  (execute-dyadic-fun-VS (vec-value tensor1) tensor2 #'.and))

(defmethod .and ((tensor1 vec) (tensor2 vec))
  (if (equal-shape? (shape tensor1) 
		    (shape tensor2))
      (execute-dyadic-fun-VV (vec-value tensor1) (vec-value tensor2) #'.and)
    (print "Error: Tensors have different sizes")))

(defmethod .and ((tensor1 matrix) (tensor2 scalar))
  (execute-dyadic-fun-MS tensor1 tensor2 #'.and))

(defmethod .and ((tensor1 matrix) (tensor2 matrix))
  (if (equal-shape? (shape tensor1)
		    (shape tensor2))
      (execute-dyadic-fun-MM tensor1 tensor2 #'.and)
    (print "Error: Tensors have different sizes")))

; Drop (drop)

(defgeneric drop (tensor1 tensor2))

(defmethod drop ((tensor1 scalar) (tensor2 vec))
  (let ((result (make-instance 'vec :value (make-array (- (length (vec-value tensor2)) (abs (scalar-value tensor1)))))))
    (if (> (scalar-value tensor1) 0)
	(loop for i from (scalar-value tensor1) below (length (vec-value tensor2))
	      do (setf (aref (vec-value result) (- i (scalar-value tensor1))) (aref (vec-value tensor2) i)))
      (loop for i from 0 below (- (length (vec-value tensor2)) (abs (scalar-value tensor1)))
	    do (setf (aref (vec-value result) i) (aref (vec-value tensor2) i))))
    result))

(defmethod drop ((tensor1 scalar) (tensor2 matrix)))

;(defmethod drop ((tensor1 vec) (tensor2 matrix))
 ; (let ((value (matrix-value tensor2))
;	(lines (* (first (matrix-dimensions tensor2))
;		  (sum-special-dims (get-special-dims (matrix-dimensions tensor2)))))
;	(remove 0))
 ;   (if (<= (length (vec-value tensor1))
;	    (length (matrix-dimensions)))
;	(progn (loop for index from (1- (length (vec-value tensor1))) to 1 by -1
;		  do (progn (setf remove (* lines (/ (aref (vec-value tensor1) index)
;						     (nth index (matrix-dimensions tensor2)))))
;			    (setf lines (- lines (abs remove)))
;			    (if (> 0 remove)
;				(loop for i from 0 below remove
;				   do (setf (aref value i) nil))
;				(loop for i from (1- (length value)) to (+ (length value) remove) by -1
;				   do (setf (aref value i) nil)))))
;	       (if (> (length (vec-value tensor1)) 1)
;		   (loop for index from 0 below (length value)
;			do (not (null 
	
			 

	


; Reshape

(defgeneric reshape (dimensions tensor))

(defmethod reshape ((dimensions vec) (tensor vec))
  (let* ((dims (make-list-from-vec (vec-value dimensions)))
         (total-dims (sum-special-dims (get-special-dims dims)))
         (vector-build (make-array (second dims) :fill-pointer 0))
         (matrix-lines (* (first dims) total-dims))
         (result (make-array matrix-lines))
         (index 0))

        (loop for line from 0 below matrix-lines 
              do (progn 
                    (loop for column from 0 below (second dims)
                      do (progn 
                            (vector-push (aref (vec-value tensor) index) vector-build)
                            (setf index (mod (+ index 1) (length (vec-value tensor))))))
                    (setf (aref result line) (make-instance 'vec :value vector-build))
                    (setf vector-build (make-array (second dims) :fill-pointer 0))))

        (make-instance 'matrix :value result :dimensions dims)))

(defmethod reshape ((dimensions vec) (tensor matrix))
   (let* ((dims (make-list-from-vec (vec-value dimensions)))
         (total-dims (sum-special-dims (get-special-dims dims)))
         (vector-build (make-array (second dims)))
         (matrix-lines (* (first dims) total-dims))
         (result (make-array matrix-lines))
         (index 0)
	 (values (matrix-values-into-list (matrix-value tensor))))
     
     (loop for line from 0 below matrix-lines 
	   do (progn 
		(loop for column from 0 below (second dims)
		      do (progn
			   (setf (aref vector-build column) (nth index values))
			   (setf index (mod (+ index 1) (length values)))))
		(setf (aref result line) (make-instance 'vec :value vector-build))
		(setf vector-build (make-array (second dims)))))
     
     (make-instance 'matrix :value result :dimensions dims)))

; Catenate (catenate)

(defgeneric catenate (tensor1 tensor2))

(defmethod catenate ((tensor1 scalar) (tensor2 scalar))
  (make-instance 'vec :value (make-array 2 :initial-contents (cons tensor1 (cons tensor2 nil)))))

(defmethod catenate ((tensor1 vec) (tensor2 vec))
  (let ((lst nil))
    (loop for index from 0 below (array-dimension (vec-value tensor1) 0)
	 do (setf lst (cons (aref (vec-value tensor1) index) lst)))
    (loop for index from 0 below (array-dimension (vec-value tensor2) 0)
	  do (setf lst (cons (aref (vec-value tensor2) index) lst)))
    (make-instance 'vec :value (make-array (list-length lst) :initial-contents (reverse lst)))))

; Member (member)

(defgeneric member? (tensor1 tensor2))

(defmethod member? ((tensor1 vec) (tensor2 scalar))
  (let ((lst nil))
    (loop for index from 0 below (array-dimension (vec-value tensor1) 0)
       if (eql (scalar-value (aref (vec-value tensor1) index)) (scalar-value tensor2))
       do (setf lst (cons (s 1) lst))
       else 
       do (setf lst (cons (s 0) lst)))
    (make-instance 'vec :value (make-array (list-length lst) :initial-contents (reverse lst)))))

(defmethod member? ((tensor1 vec) (tensor2 vec))
  (let ((result (make-instance 'vec :value (make-array (length (vec-value tensor1)))))
	(exists nil))
    (loop for i from 0 below (length (vec-value tensor1))
	  do (progn (loop for j from 0 below (length (vec-value tensor2))
			  do (if (eql (scalar-value (aref (vec-value tensor1) i))
				      (scalar-value (aref (vec-value tensor2) j)))
				 (progn (setf exists T)
					(return))))
		    (if exists
			(setf (aref (vec-value result) i) (s 1))
		      (setf (aref (vec-value result) i) (s 0)))))
    result))
	
			       

; Select (select)

(defgeneric select (tensor1 tensor2))

(defmethod select ((tensor1 vec) (tensor2 vec))
  (let ((lst nil))
    (if (equal-shape? (shape tensor1)
		      (shape tensor2))
	(progn (loop for index from 0 below (length (vec-value tensor1))
		     do (if (eql 1
				 (scalar-value (aref (vec-value tensor1) index)))
				   (setf lst (cons (aref (vec-value tensor2) index) lst))))
	
	       (make-instance 'vec :value (make-array (list-length lst) :initial-contents (reverse lst)))))))

(defmethod select ((tensor1 vec) (tensor2 matrix))
  (let* ((lines (first (matrix-dimensions tensor2)))
	 (result (make-array lines)))
    (loop for i from 0 below lines
	  do (setf (aref result i) (select tensor1 (aref (matrix-value tensor2) (+ i (- (length (matrix-value tensor2)) lines))))))
    (make-instance 'matrix :value result :dimensions (list lines (length (vec-value (aref result 0)))))))
    

(defmethod select ((tensor1 matrix) (tensor2 matrix))
  (let* ((lines (first (matrix-dimensions tensor2)))
	 (result (make-array lines)))
    (if (equal-shape? (shape tensor1)
		      (shape tensor2))
	 (loop for i from 0 below lines
	       do (setf (aref result i) (select (aref (matrix-value tensor1) (+ i (- (length (matrix-value tensor1)) lines)))
						(aref (matrix-value tensor2) (+ i (- (length (matrix-value tensor2)) lines))))))
      (make-instance 'matrix :value result :dimensions (list lines (length (vec-value (aref result 0))))))))

; OPERATORS

; Monadic Operators


(defun fold (fun)
  (lambda (tensor)
    (if (eql 0 (length (vec-value tensor)))
	      (v 0)
      (let ((result (aref (vec-value tensor) 0)))
	(loop for index from 1 below (array-dimension (vec-value tensor) 0)
	      do (setf result (funcall fun result (aref (vec-value tensor) index))))
	(v (scalar-value result))))))

(defun scan (fun)
  (lambda (tensor)
    (let ((lst (cons (aref (vec-value tensor) 0) nil))
	  (iteration 1))
      (loop for i from 1 below (array-dimension (vec-value tensor) 0)
	    do (let ((result (aref (vec-value tensor) 0)))
		 (loop for j from 1 to iteration
		       do (setf result (funcall fun result (aref (vec-value tensor) j))))
		 (incf iteration)
		 (setf lst (cons result lst))))
      (make-instance 'vec :value (make-array (list-length lst) :initial-contents (reverse lst))))))   
  

(defun outer-product (fun)
  (lambda (tensor1 tensor2)
    (outer-product-aux tensor1 tensor2 fun)))
   

(defgeneric outer-product-aux (tensor1 tensor2 fun))

(defmethod outer-product-aux ((tensor1 scalar) (tensor2 scalar) fun)
  (funcall fun tensor1 tensor2))

(defmethod outer-product-aux ((tensor1 scalar) (tensor2 vec) fun)
  (let ((result (make-array (length (vec-value tensor2)))))
    (loop for index from 0 below (length (vec-value tensor2))
	  do (setf (aref result index) (funcall fun tensor1 (aref (vec-value tensor2) index))))
    (make-instance 'vec :value result)))

(defmethod outer-product-aux ((tensor1 scalar) (tensor2 matrix) fun)
  (let ((result (make-array (length (matrix-value tensor2))))
	(vec (make-array (first (matrix-dimensions tensor2)))))
    (loop for i from 0 below (length (matrix-value tensor2))
	  do (progn (loop for j from 0 below (first (matrix-dimensions tensor2))
			  do (setf (aref vec j) (funcall fun tensor1 (aref (vec-value (aref (matrix-value tensor2) i)) j))))
		    (setf (aref result i) vec)
		    (setf vec (make-array (first (matrix-dimensions tensor2))))))
    (make-instance 'matrix :value result :dimensions (matrix-dimensions tensor2))))

(defmethod outer-product-aux ((tensor1 vec) (tensor2 scalar) fun)
   (let ((result (make-array (length (vec-value tensor1)))))
    (loop for index from 0 below (length (vec-value tensor1))
	  do (setf (aref result index) (funcall fun (aref (vec-value tensor1) index) tensor2)))
    (make-instance 'vec :value result)))

(defmethod outer-product-aux ((tensor1 vec) (tensor2 vec) fun)
  (let ((result (make-array (length (vec-value tensor1))))
	(vec (make-array (length (vec-value tensor2)))))
    (loop for i from 0 below (length (vec-value tensor1))
          do (progn 
	       (loop for j from 0 below (length (vec-value tensor2))
		     do (setf (aref vec j) (funcall fun (aref (vec-value tensor1) i) (aref (vec-value tensor2) j))))
	       (setf (aref result i) (make-instance 'vec :value vec))
	       (setf vec (make-array (length (vec-value tensor2))))))
    (make-instance 'matrix :value result :dimensions (make-list-from-vec (vec-value (catenate (shape tensor1) (shape tensor2)))))))

(defmethod outer-product-aux ((tensor1 vec) (tensor2 matrix) fun)
	(let ((result (make-array (* (length (vec-valu tensor1)) (first (matrix-dimensions tensor2)))))
		  (line (make-array (second (matrix-dimensions tensor2))))
		  (index 0))

		(loop for element from 0 below (length (vec-value tensor1))
			do (loop for i from 0 below (first (matrix-dimensions tensor2))
				do (progn
						(loop for j from 0 below (second (matrix-dimensions tensor2))
							do (setf (aref line j) (funcall fun (aref (vec-value tensor1) element) (aref (vec-value (aref (matrix-value tensor2) i)) j))))
						(setf (aref result index) (make-instance 'vec :value line))
						(incf index)
						(setf line (make-array (second (matrix-dimensions tensor2)))))
				)
			)
		(make-instance 'matrix :value result :dimensions (make-list-from-vec (vec-value (catenate (shape tensor2) (shape tensor1)))) )
		)
	)

(defmethod outer-product-aux ((tensor1 matrix) (tensor2 scalar) fun)
    (let ((result (make-array (length (matrix-value tensor1))))
	  (vec (make-array (first (matrix-dimensions tensor1)))))
      (loop for i from 0 below (length (matrix-value tensor1))
	    do (progn (loop for j from 0 below (first (matrix-dimensions tensor1))
			    do (setf (aref vec j) (funcall fun (aref (vec-value (aref (matrix-value tensor1) i)) j) tensor2)))
		      (setf (aref result i) vec)
		      (setf vec (make-array (first (matrix-dimensions tensor1))))))
      (make-instance 'matrix :value result :dimensions (matrix-dimensions tensor1))))

(defmethod outer-product-aux ((tensor1 matrix) (tensor2 vec) fun))

(defmethod outer-product-aux ((tensor1 matrix) (tensor2 matrix) fun)
  (let* ((new-lines (* (first (matrix-dimensions tensor2))
		   (sum-special-dims (matrix-dimensions tensor1))))
	 (vec (make-array (second (matrix-dimensions tensor2))))
	 (line1 (* (first (matrix-dimensions tensor1))
		   (sum-special-dims (get-special-dims (matrix-dimensions tensor1)))))
	 (line2 (* (first (matrix-dimensions tensor2))
		   (sum-special-dims (get-special-dims (matrix-dimensions tensor2)))))
	 (value (make-array new-lines))
	 (new-dims (append (matrix-dimensions tensor2) (matrix-dimensions tensor1)))
	 (index 0))
    (loop for line from 0 below line1
	  do (loop for column from 0 below (second (matrix-dimensions tensor1))
		   do  (loop for i from 0 below line2
			     do (progn (loop for j from 0 below (second (matrix-dimensions tensor2))
					     do (setf (aref vec j) (funcall fun (aref (vec-value (aref (matrix-value tensor1) line)) column)
									    (aref (vec-value (aref (matrix-value tensor2) i)) j))))
				       (setf (aref value index) (make-instance 'vec :value vec))
				       (incf index)
				       (setf vec (make-array (second (matrix-dimensions tensor2))))))))
    (make-instance 'matrix :value value :dimensions new-dims)))
			    
		   
  

  
; Dyadic Operators

(defun inner-product (f1 f2)
  (lambda (tensor1 tensor2)
    (inner-product-aux tensor1 tensor2 f1 f2)))

(defgeneric inner-product-aux (tensor1 tensor2 f1 f2))

(defmethod inner-product-aux ((tensor1 scalar) (tensor2 vec) f1 f2)
  (let ((result (funcall f2 tensor1 (aref (vec-value tensor2) 0))))
    (loop for i from 1 below (length (vec-value tensor2))
	  do (setf result (funcall f1 result (funcall f2 tensor1 (aref (vec-value tensor2) i)))))
    result))

(defmethod inner-product-aux ((tensor1 scalar) (tensor2 matrix) f1 f2)
  (let ((result (make-array (length (matrix-value tensor2))))
	(vec nil)
	(partial-result 0))
    (loop for i from 0 below (length (matrix-value tensor2))
	  do (progn (setf partial-result (funcall f2 tensor1 (aref (vec-value (aref (matrix-value tensor2) i)) 0)))
		    (setf vec (make-array 1))
		    (loop for j from 1 below (length (vec-value (aref (matrix-value tensor2) i)))
			  do (setf partial-result (funcall f1 partial-result (funcall f2 tensor1 (aref (vec-value (aref (matrix-value tensor2) i)) j)))))
		    (setf (aref vec 0) partial-result)
		    (setf (aref result i) (make-instance 'vec :value vec))))
    (make-instance 'matrix :value result :dimensions (list (first (matrix-dimensions tensor2)) 1))))

(defmethod inner-product-aux ((tensor1 vec) (tensor2 scalar) f1 f2)
  (let ((result (funcall f2 (aref (vec-value tensor1) 0) tensor2)))
    (loop for i from 1 below (length (vec-value tensor1))
	  do (setf result (funcall f1 result (funcall f2 (aref (vec-value tensor1) i) tensor2))))
    result))

(defmethod inner-product-aux ((tensor1 vec) (tensor2 vec) f1 f2)
  (let ((result (make-array (length (vec-value tensor1))))
	(partial-result 0))
    (loop for i from 0 below (length (vec-value tensor1))
	  do (progn (setf partial-result (funcall f2 (aref (vec-value tensor1) i) (aref (vec-value tensor2) 0)))
		    (loop for j from 1 below (length (vec-value tensor2))
			  do (setf partial-result (funcall f1 partial-result (funcall f2 (aref (vec-value tensor1) i) (aref (vec-value tensor2) j)))))
		    (setf (aref result i) partial-result)))
    (make-instance 'vec :value result)))
			  

(defmethod inner-product-aux ((tensor1 vec) (tensor2 matrix) f1 f2)
  (let ((result (make-array (second (matrix-dimensions tensor2))))
	(partial-result 0))
    (loop for c from 0 below (second (matrix-dimensions tensor2))
	  do (progn (setf partial-result (funcall f2 (aref (vec-value tensor1) 0)
						  (aref (vec-value (aref (matrix-value tensor2) 0)) c)))
		    (loop for j from 1 below (first (matrix-dimensions tensor2))
			  do (setf partial-result (funcall f1 partial-result
							      (funcall f2 (aref (vec-value tensor1) j)
						                	   (aref (vec-value (aref (matrix-value tensor2) j)) c)))))
		    (setf (aref result c) partial-result)))
    (make-instance 'vec :value result)))

(defmethod inner-product-aux ((tensor1 matrix) (tensor2 scalar) f1 f2)
  (let ((result (make-array (length (matrix-value tensor1))))
	(vec nil)
	(partial-result 0))
    (loop for i from 0 below (length (matrix-value tensor1))
	  do (progn (setf partial-result (funcall f2 (aref (vec-value (aref (matrix-value tensor1) i)) 0) tensor2))
		    (setf vec (make-array 1))
		    (loop for j from 1 below (length (vec-value (aref (matrix-value tensor1) i)))
			  do (setf partial-result (funcall f1 partial-result (funcall f2 (aref (vec-value (aref (matrix-value tensor1) i)) j) tensor2))))
		    (setf (aref vec 0) partial-result)
		    (setf (aref result i) (make-instance 'vec :value vec))))
    (make-instance 'matrix :value result :dimensions (list (first (matrix-dimensions tensor1)) 1))))

(defmethod inner-product-aux ((tensor1 matrix) (tensor2 vec) f1 f2)
  (let ((result (make-array (second (matrix-dimensions tensor1))))
	(partial-result 0))
    (loop for c from 0 below (second (matrix-dimensions tensor1))
	  do (progn (setf partial-result (funcall f2 (aref (vec-value (aref (matrix-value tensor1) 0)) c)
						  (aref (vec-value tensor2) 0)))
		    (loop for j from 1 below (first (matrix-dimensions tensor1))
			  do (setf partial-result (funcall f1 partial-result
							   (funcall f2 (aref (vec-value (aref (matrix-value tensor1) j)) c)
								    (aref (vec-value tensor2) j)))))
		    (setf (aref result c) partial-result)))
    (make-instance 'vec :value result)))

(defmethod inner-product-aux ((tensor1 matrix) (tensor2 matrix) f1 f2)
  (let* ((result (make-array (second (matrix-dimensions tensor1))))
	 (vec (make-array (second (matrix-dimensions tensor2))))
	 (partial-result 0)
	 (col1 1)
	 (col2 0))
    (loop for l from 0 below (second (matrix-dimensions tensor1))
	  do (progn (loop for c from 0 below (second (matrix-dimensions tensor2))
			  do (progn (setf partial-result (funcall f2 (aref (vec-value (aref (matrix-value tensor1) l)) 0)
								  (aref (vec-value (aref (matrix-value tensor2) 0)) col2)))
		    
				    (loop for i from 1 below (first (matrix-dimensions tensor2))
					  do (progn
					       (setf partial-result (funcall f1 partial-result
										     (funcall f2 (aref (vec-value (aref (matrix-value tensor1) l)) col1)
											         (aref (vec-value (aref (matrix-value tensor2) i)) col2))))
						    (incf col1)))
				    (setf (aref vec c) partial-result)
				    (setf col1 1)
				    (incf col2)))
		    (setf (aref result l) (make-instance 'vec :value vec))
		    (setf vec (make-array (second (matrix-dimensions tensor2))))
		    (setf col2 0)))
    (make-instance 'matrix :value result :dimensions (append (list (first (matrix-dimensions tensor1))
								   (second (matrix-dimensions tensor2)))))))
							     
								   
		    

; Exercises


; 1. Tally

(defun tally (arg)
  (funcall (fold #'.*) (shape arg)))

; 2. Rank

(defun rank (arg)
  (funcall (fold #'.+) (member? (shape arg) (shape arg))))

; 3. Within

(defun within (vec min max)
  (select (.and (funcall (outer-product #'.>=)  vec min) (funcall (outer-product #'.<=) vec max)) vec))

; 4. Ravel

(defun ravel (tensor)
  (reshape
        (catenate (v 1) (tally tensor))
        tensor)
)

; Auxiliary Functions

(defun execute-monadic-fun-V (vec fun)
  (let ((result (make-array (length vec))))
    (loop for index from 0 below (length vec)
       do (setf (aref result index) (funcall fun (aref vec index))))
    (make-instance 'vec :value result)))

(defun execute-monadic-fun-M (m fun)
  (let ((result (make-array (length (matrix-value m)))))
    (loop for index from 0 below (length (matrix-value m))
       do (setf (aref result index) (execute-monadic-fun-V (vec-value (aref (matrix-value m) index)) fun)))
    (make-instance 'matrix :value result :dimensions (matrix-dimensions m))))

(defun execute-dyadic-fun-VV (vec1 vec2 fun)
  (let ((result (make-array (length vec1))))
    (loop for index from 0 below (length vec1)
       do (setf (aref result index) (funcall fun (aref vec1 index)
			                         (aref vec2 index))))
    (make-instance 'vec :value result)))

(defun execute-dyadic-fun-SV (sca vec fun)
  (let ((result (make-array (length vec))))
    (loop for index from 0 below (array-dimension vec 0)
	 do (setf (aref result index) (funcall fun sca (aref vec index))))
    (make-instance 'vec :value result)))

(defun execute-dyadic-fun-VS (vec sca fun)
  (let ((result (make-array (length vec))))
    (loop for index from 0 below (length vec)
	  do (setf (aref result index) (funcall fun (aref vec index) sca)))
    (make-instance 'vec :value result)))

(defun execute-dyadic-fun-SM (sca m fun)
  (let ((result (make-array (length (matrix-value m)))))
    (loop for index from 0 below (length (matrix-value m))
	  do (setf (aref result index) (execute-dyadic-fun-SV sca (vec-value (aref (matrix-value m) index)) fun)))
    (make-instance 'matrix :value result :dimensions (matrix-dimensions m))))

(defun execute-dyadic-fun-MS (m sca fun)
  (let ((result (make-array (length (matrix-value m)))))
    (loop for index from 0 below (length (matrix-value m))
	  do (setf (aref result index) (execute-dyadic-fun-VS (vec-value (aref (matrix-value m) index)) sca fun)))
    (make-instance 'matrix :value result :dimensions (matrix-dimensions m))))

(defun execute-dyadic-fun-MM (m1 m2 fun)
  (let ((result (make-array (length (matrix-value m1)))))
    (loop for index from 0 below (length (matrix-value m1))
	  do (setf (aref result index) (execute-dyadic-fun-VV (vec-value (aref (matrix-value m1) index))
							   (vec-value (aref (matrix-value m2) index))
							   fun)))
    (make-instance 'matrix :value result :dimensions (matrix-dimensions m1))))
	  

(defun make-list-from-vec (vec)
  (let ((result (list)))
    (loop for i from 0 below (length vec)
      do (setf result (append result (list (scalar-value (aref vec i))))))
    result
    ))

(defun sum-special-dims (dims)
  (let ((sum 1))
    (if (null dims)
      1
      (dolist (dim dims)
        (setf sum (* sum dim))))
    sum))


(defun get-special-dims (dims)
  (if (null (cddr dims))
    (list 1)
    (cddr dims)))

(defun equal-shape? (shape1 shape2)
  (let ((shape T))
    (if (equal (length (vec-value shape1))
	     (length (vec-value shape2)))
	(loop for index from 0 below (length (vec-value shape1))
	      do (if (not (eql (scalar-value (aref (vec-value shape1) index))
			       (scalar-value (aref (vec-value shape2) index))))
		     (progn
		       (setf shape nil)
		       (return))))
      (setf shape nil))
    shape))

(defun matrix-values-into-list (values)
  (let ((lst nil))
    (loop for i from 0 below (length values)
	  do (loop for j from 0 below (length (vec-value (aref values i)))
		   do (setf lst (append lst (list (aref (vec-value (aref values i)) j))))))
    lst))
		     
