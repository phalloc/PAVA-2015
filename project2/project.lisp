
(defgeneric display-tensor (tensor) )

;to correct
(defmethod display-tensor ((tensor vector))
	(loop for index from 0 to (- (length tensor) 1)
		do (format t "~A " (aref tensor index))))


;SCALARS AND VECTORS
(defun s (x)
	x)

(defun v (&rest args)
	(let ((vec (make-array (length args) :initial-contents args :adjustable t)))
	  (display-tensor vec)
	  vec))
		

		
;MONADIC FUNCTIONS


; Symmetric (.-)
(defgeneric .- (tensor) )

(defmethod .- ((tensor number))
  (* -1 tensor))

(defmethod .- ((tensor vector))
  (let ((result (make-array (length tensor))))
    (loop for index from 0 to (- (length tensor) 1)
       do (setf (aref result index) (.- (aref tensor index))))
    result))
    
; Inverse (./)

(defgeneric ./ (tensor) )

(defmethod ./ ((tensor number))
  (/ 1 tensor))

(defmethod ./ ((tensor vector))
  (let ((result (make-array (length tensor))))
    (loop for index from 0 to (- (length tensor) 1)
       do (setf (aref result index) (./ (aref tensor index))))
    result))
		
; Factorial (.!)

(defun fact (n)
  (if (eql n 0) 
      1
      (* n (fact (- n 1)))))

(defgeneric .! (tensor) )

(defmethod .! ((tensor number))
  (fact tensor))

(defmethod .! ((tensor vector))
  (let ((result (make-array (length tensor))))
    (loop for index from 0 to (- (length tensor) 1)
       do (setf (aref result index) (.! (aref tensor index))))
    result))

; sin (.sin)

(defgeneric .sin (tensor))

(defmethod .sin ((tensor number))
  (sin tensor))

(defmethod .sin ((tensor vector))
  (let ((result (make-array (length tensor))))
    (loop for index from 0 to (- (length tensor) 1)
       do (setf (aref result index) (.sin (aref tensor index))))
    result))

; cos (.cos)

(defgeneric .cos (tensor))

(defmethod .cos ((tensor number))
  (cos tensor))

(defmethod .cos ((tensor vector))
  (let ((result (make-array (length tensor))))
    (loop for index from 0 to (- (length tensor) 1)
       do (setf (aref result index) (.cos (aref tensor index))))
    result))

; not (.not)

(defgeneric .not (tensor) )

(defmethod .not ((tensor number))
  (if (eql tensor 0)
      1
      0))

(defmethod .not ((tensor vector))
  (let ((result (make-array (length tensor))))
    (loop for index from 0 to (- (length tensor) 1)
       do (setf (aref result index) (.not (aref tensor index))))
    result))

; Reshape


; Shape

(defgeneric shape (tensor))

(defmethod shape ((tensor number))
  0)

(defmethod shape ((tensor vector))
  (length tensor))

; Interval

(defun interval (n)
  (let ((result (make-array n)))
    (loop for index from 0 to (- n 1)
	 do (setf (aref result index)
		  (+ index 1)))
    result))

; DYADIC FUNCTIONS


; Sum (.+)

(defgeneric .+ (tensor1 tensor2))

(defmethod .+ ((tensor1 number) (tensor2 number))
  (+ tensor1 tensor2))

(defmethod .+ ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.+ tensor1 (aref tensor2 index))))
    result))

(defmethod .+ ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.+ tensor2 (aref tensor1 index))))
    result))

(defmethod .+ ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.+ (aref tensor1 index)
			(aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))


; Sub (.-)

;(defgeneric .- (tensor1 tensor2))

;(defmethod .- ((tensor1 number) (tensor2 number))
;  (- tensor1 tensor2))

;(defmethod .- ((tensor1 number) (tensor2 vector))
;  (let ((result (make-array (length tensor2))))
;    (loop for index from 0 to (- (length tensor2) 1)
;       do (setf (aref result index) (.- tensor1 (aref tensor2 index))))
;    result))

;(defmethod .- ((tensor1 vector) (tensor2 number))
;  (let ((result (make-array (length tensor1))))
;    (loop for index from 0 to (- (length tensor1) 1)
;       do (setf (aref result index) (.- tensor2 (aref tensor1 index))))
;    result))


; Mul (.*)

(defgeneric .* (tensor1 tensor2))

(defmethod .* ((tensor1 number) (tensor2 number))
  (* tensor1 tensor2))

(defmethod .* ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.* tensor1 (aref tensor2 index))))
    result))

(defmethod .* ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.* tensor2 (aref tensor1 index))))
    result))

(defmethod .* ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.* (aref tensor1 index)
			(aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))

; Div (./)

;(defgeneric ./ (tensor1 tensor2))

;(defmethod ./ ((tensor1 number) (tensor2 number))
;  (/ tensor1 tensor2))

;(defmethod ./ ((tensor1 number) (tensor2 vector))
;  (let ((result (make-array (length tensor2))))
;    (loop for index from 0 to (- (length tensor2) 1)
;       do (setf (aref result index) (./ tensor1 (aref tensor2 index))))
;    result))

;(defmethod ./ ((tensor1 vector) (tensor2 number))
;  (let ((result (make-array (length tensor1))))
;    (loop for index from 0 to (- (length tensor1) 1)
;       do (setf (aref result index) (./ tensor2 (aref tensor1 index))))
;    result))

;(defmethod ./ ((tensor1 vector) (tensor2 vector))
;  (if (eql (shape tensor1) 
;	   (shape tensor2))
;      (let ((result (make-array (length tensor1))))
;	(loop for index from 0 to (- (length tensor1) 1)
;	   do (setf (aref result index)
;		    (./ (aref tensor1 index)
;			(aref tensor2 index))))
;	result)
;      (print "Error: Tensors have different sizes")))

; Integer Division (.//)

(defgeneric .// (tensor1 tensor2))

(defmethod .// ((tensor1 number) (tensor2 number))
  (let ((result (floor tensor1 tensor2)))
    result))

(defmethod .// ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.// tensor1 (aref tensor2 index))))
    result))

(defmethod .// ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.// tensor2 (aref tensor1 index))))
    result))

(defmethod .// ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.// (aref tensor1 index)
			 (aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))

; Remainder (.%)

(defgeneric .% (tensor1 tensor2))

(defmethod .% ((tensor1 number) (tensor2 number))
  (mod tensor1 tensor2))

(defmethod .% ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.% tensor1 (aref tensor2 index))))
    result))

(defmethod .% ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.% tensor2 (aref tensor1 index))))
    result))

(defmethod .% ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.% (aref tensor1 index)
			(aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))

; Lesser than (.<)

(defgeneric .< (tensor1 tensor2))

(defmethod .< ((tensor1 number) (tensor2 number))
  (if (< tensor1 tensor2)
      1
      0))

(defmethod .< ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.< tensor1 (aref tensor2 index))))
    result))

(defmethod .< ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.< tensor2 (aref tensor1 index))))
    result))

(defmethod .< ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.< (aref tensor1 index)
			(aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))

; Greater than (.>)

(defgeneric .> (tensor1 tensor2))

(defmethod .> ((tensor1 number) (tensor2 number))
  (if (> tensor1 tensor2)
      1
      0))

(defmethod .> ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.> tensor1 (aref tensor2 index))))
    result))

(defmethod .> ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.> tensor2 (aref tensor1 index))))
    result))

(defmethod .> ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.> (aref tensor1 index)
			(aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))

; Lesser or Equal than (.<=)

(defgeneric .<= (tensor1 tensor2))

(defmethod .<= ((tensor1 number) (tensor2 number))
  (if (<= tensor1 tensor2)
      1
      0))

(defmethod .<= ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.<= tensor1 (aref tensor2 index))))
    result))

(defmethod .<= ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.<= tensor2 (aref tensor1 index))))
    result))

(defmethod .<= ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.<= (aref tensor1 index)
			 (aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))

; Greater or Equal than (.<=)

(defgeneric .>= (tensor1 tensor2))

(defmethod .>= ((tensor1 number) (tensor2 number))
  (if (>= tensor1 tensor2)
      1
      0))

(defmethod .>= ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.>= tensor1 (aref tensor2 index))))
    result))

(defmethod .>= ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.>= tensor2 (aref tensor1 index))))
    result))

(defmethod .>= ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.>= (aref tensor1 index)
			 (aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))

; Equal (.=)

(defgeneric .= (tensor1 tensor2))

(defmethod .= ((tensor1 number) (tensor2 number))
  (if (= tensor1 tensor2)
      1
      0))

(defmethod .= ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.= tensor1 (aref tensor2 index))))
    result))

(defmethod .= ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.= tensor2 (aref tensor1 index))))
    result))

(defmethod .= ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.= (aref tensor1 index)
			(aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))

; Or (.or)

(defgeneric .or (tensor1 tensor2))

(defmethod .or ((tensor1 number) (tensor2 number))
  (if (or tensor1 tensor2)
      1
      0))

(defmethod .or ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.or tensor1 (aref tensor2 index))))
    result))

(defmethod .or ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.or tensor2 (aref tensor1 index))))
    result))

(defmethod .or ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.or (aref tensor1 index)
			 (aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))


; And (.and)

(defgeneric .and (tensor1 tensor2))

(defmethod .and ((tensor1 number) (tensor2 number))
  (if (= tensor1 tensor2)
      1
      0))

(defmethod .and ((tensor1 number) (tensor2 vector))
  (let ((result (make-array (length tensor2))))
    (loop for index from 0 to (- (length tensor2) 1)
       do (setf (aref result index) (.and tensor1 (aref tensor2 index))))
    result))

(defmethod .and ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       do (setf (aref result index) (.and tensor2 (aref tensor1 index))))
    result))

(defmethod .and ((tensor1 vector) (tensor2 vector))
  (if (eql (shape tensor1) 
	   (shape tensor2))
      (let ((result (make-array (length tensor1))))
	(loop for index from 0 to (- (length tensor1) 1)
	   do (setf (aref result index)
		    (.and (aref tensor1 index)
			  (aref tensor2 index))))
	result)
      (print "Error: Tensors have different sizes")))

; Drop (drop)



; Reshape (reshape)

; Catenate (catenate)

(defgeneric catenate (tensor1 tensor2))

(defmethod catenate ((tensor1 number) (tensor2 number))
  (vector tensor1 tensor2))

(defmethod catenate ((tensor1 vector) (tensor2 vector))
  (let ((result (make-array (+ (length tensor1) (length tensor2)) :fill-pointer 0))) 
    (loop for index from 0 to (- (length tensor1) 1)
	 do (vector-push (aref tensor1 index) result))
    (loop for index from 0 to (- (length tensor2) 1)
	 do (vector-push (aref tensor2 index) result))
    result))

; Member (member)

(defgeneric member? (tensor1 tensor2))

(defmethod member? ((tensor1 vector) (tensor2 number))
  (let ((result (make-array (length tensor1))))
    (loop for index from 0 to (- (length tensor1) 1)
       if (eql (aref tensor1 index) tensor2)
       do (setf (aref result index) 1)
	 else 
	 do (setf (aref result index) 0))
    result))

