(in-package #:ol-user)

(defun transposition (i j)
  #f
  (declare (type fixnum i j))
  (lambda (x)
    (declare (type fixnum x))
    (cond
      ((= i x) j)
      ((= j x) i)
      (t x))))

(defun compile-permutation (n p)
  (let ((mapping (make-array n :element-type 'fixnum)))
    (fill-array (:fill) mapping (indices i (funcall p i)))
    (lambda (x) (aref mapping x))))

(defun enumerate-permutations (n transposer)
  #f
  (declare (fixnum n))
  (let ((a (make-array (the fixnum (1+ n))
                       :initial-element 0
                       :element-type 'fixnum))
        (x 0) (y 0)
        (m 2))
    (declare (fixnum x y m))
    (tagbody
     start
       (incf (aref a m))
       ;; (break "start: m = ~A, a_~A = ~A" m m (aref a m))
       (if (= (aref a m) m)
           (go next-column)
           (go next-entry))

     next-entry
       (setf x m)
       (setf y (if (evenp m)
                   (aref a m)
                   1))
       ;; x und y enthalten die Indizes, die als nächstes vertauscht
       ;; werden sollen.
       ;; (format t "(~A ~A)  " x y)
       (funcall transposer x y)
       ;; (break "next-entry: x = m = ~A, a_~A = ~A, y = ~A" m m (aref a m) y)
       (setf m 2)
       (go start)

     next-column
       (setf (aref a m) 0)
       (incf m)
       ;; (break "next-column: m = ~A, n = ~A" m n)
       (unless (> m n)
         (go start)))))

(defun enumerate-permutations (n transposer)
  #f
  (declare (fixnum n))
  (let ((a (make-array (the fixnum (1+ n))
                       :initial-element 0
                       :element-type 'fixnum))
        (m 2))
    (declare (fixnum m))
    (tagbody
     start
       (incf (aref a m))
       ;; (break "start: m = ~A, a_~A = ~A" m m (aref a m))
       (if (= (aref a m) m)
           (go next-column)
           (go next-entry))

     next-entry
       (funcall transposer m
                (if (evenp m)
                    (aref a m)
                    1))
       (setf m 2)
       (go start)

     next-column
       (setf (aref a m) 0)
       (incf m)
       ;; (break "next-column: m = ~A, n = ~A" m n)
       (unless (> m n)
         (go start)))))

(defun all-permutations (n)
  (let* ((mapping (arange n))
         (permutations (list mapping)))
    (when (> n 1)
     (enumerate-permutations
      n
      (lambda (i j)
       (push (setf mapping
                    (map 'vector
                         (transposition (- i 1)
                                        (- j 1))
                         mapping))
              permutations))))
    (nreverse permutations)))

(defun last-permutation (n)
  (let ((mapping (arange n)))
    (enumerate-permutations n
                            (lambda (i j)
                              (rotatef (aref mapping (- i 1))
                                       (aref mapping (- j 1)))))
    mapping))

(defun has-duplicates (sequence)
  (not (= (length sequence)
      (length 
       (remove-duplicates sequence :test #'equal)))))