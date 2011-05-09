(in-package #:ccb)

;;; Rank and Unrank permutations
(defun id-perm (n)
  (let ((perm (make-array n)))
    (fill-array (:fill) perm (indices i (1+ i)))
    perm))

(defun inverse (perm)
  (let ((inv (make-array (length perm))))
    (loop for j across perm
       and i from 1 do
         (setf (aref inv (1- j)) i))
    inv))

(defun preimage (perm k)
  (1+  (position k perm)))

(defun image (perm k)
  (aref perm (1- k)))


(defun permutation-rank-rec (perm)
  "The inductive definition of the rank function which can easily be
verified to be correct."
  (let ((n (length perm)))
    (if (= n 1) 0
        (let ((j (preimage perm n))
              (r (rank (remove n perm))))
          (+ (* n r)
             (if (oddp r)
                 (- j 1)
                 (- n j)))))))

(defun permutation-rank (perm &optional n (factor 1) (odd 0) (even 0))
  "A tail recursive version of the rank function, which relies on some
parity relation in the above recursive definition.  N holds the number
of permuted elements, FACTOR is sort of a factorial.  ODD holds the
offset to PERM if PERM's rank is odd, EVEN resp. for even rank."
  (unless n
    (setf n (length perm)))
  (if (= n 1) even
      (let ((j (preimage perm n))
            (p (remove n perm)))
        (cond ((evenp (- n j))
               (permutation-rank p (- n 1) (* n factor)
                                 (+ odd  (* factor (- j 1)))
                                 (+ even (* factor (- n j)))))
              ((oddp (- n j))
               (permutation-rank p (- n 1) (* n factor)
                                 (+ even (* factor (- j 1)))
                                 (+ odd  (* factor (- n j)))))))))

(defun permutation-rank-fast (perm &optional (n -1) (factor 1) (odd 0) (even 0))
  "A tail recursive version of the rank function, which relies on some
parity relation in the above recursive definition.  N holds the number
of permuted elements, FACTOR is sort of a factorial.  ODD holds the
offset to PERM if PERM's rank is odd, EVEN resp. for even rank."
  #f  (declare (fixnum n factor odd even) (simple-array perm))
  (when (minusp n)
    (setf n (length perm)))
  (if (= n 1) even
      (let ((j (preimage perm n))
            (p (remove n perm)))
        (declare (fixnum j) (simple-array p))
        (cond ((evenp (- n j))
               (permutation-rank-fast p (- n 1) (the fixnum (* n factor))
                                 (+ odd  (the fixnum (* factor (- j 1))))
                                 (+ even (the fixnum (* factor (- n j))))))
              ((oddp (- n j))
               (permutation-rank-fast p (- n 1) (the fixnum (* n factor))
                                 (+ even (the fixnum (* factor (- j 1))))
                                 (+ odd  (the fixnum (* factor (- n j))))))))))


(defun rank-test (n)
  (let ((map (amrange 1 n)))
    #1= (format t "~A .. ~2D .. ~2D~%" map (rank map) (rank-rec map))
    (lambda (i j)
      (rotatef (aref map (1- i))
               (aref map (1- j)))
      #1#)))

(defun rotate-left (array i j)
  (assert (< i j))
  (let ((tmp (aref array (1- i))))
    (loop for k from i below j do
         (setf (aref array (1- k))
               (aref array k)))
    (setf (aref array (1- j)) tmp))
  array)

(defun rotate-right (array i j)
  (assert (< i j))
  (let ((tmp (aref array (1- j))))
    (loop for k from (1- j) downto i do
         (setf (aref array k)
               (aref array (1- k))))
    (setf (aref array (1- i)) tmp))
  array)

(defun rotate (array i j)
  "Rotate the interval [i, j] in array s.t. place i goes to place j.
Indices start with 1."
  (cond ((< i j) (rotate-left array i j))
        ((> i j) (rotate-right array j i))
        (t array))) ; do nothing


(defun permutation-unrank-rec (n r &optional (perm (id-perm n)))
  "A recursive inverse of the function permutation-rank (where N is
the length of PERM)."
  (unless (= n 1)
   (multiple-value-bind (rr m) (floor r n)
     (let ((j (if (oddp rr) (+ m 1) (- n m))))
      ;; rotate the first places
       (permutation-unrank-rec (1- n) rr perm)
       (rotate perm n j))))
  perm)

(defun set-next-zero (array index value)
  "Put the value into array at the next position after index that has
currently zero value.  If all those position are non-zero, do
nothing."
  #f
  (declare (fixnum index value) ((simple-array fixnum (*)) array))
  (do ((i index (1+ i))
       (l (length array)))
      ((or (>= i l)
           (when (zerop (aref array i))
             (setf (aref array i) value)
             t)))
      (declare (fixnum i l)))
  value)

(defsetf next-zero set-next-zero)

(defun permutation-unrank (n r &optional
                           (perm (make-array n :initial-element 0)))
  "A tail recursive inverse of the function permutation-rank (where N is
the length of PERM)."
  (if (= n 1)
      (progn
        (setf (next-zero perm 0) 1)
        perm)
      (multiple-value-bind (rr m) (floor r n)
        (let ((j (if (oddp rr) (+ m 1) (- n m))))
          (setf (next-zero perm (1- j)) n))
        (permutation-unrank (1- n) rr perm))))

(defun permutation-unrank-fast (n r &optional
                           (perm (make-array n :element-type 'fixnum
                                             :initial-element 0)))
  #f
  (declare (fixnum n r) ((simple-array fixnum (*)) perm))
  (if (= n 1)
      (progn
        (setf (next-zero perm 0) 1)
        perm)
      (multiple-value-bind (rr m) (floor r n)
        (declare (fixnum rr m))
        (let ((j (if (oddp rr) (+ m 1) (- n m))))
          (declare (fixnum j))
          (setf (next-zero perm (the fixnum (1- j))) n))
        (permutation-unrank-fast (1- n) rr perm))))

(defun factorial (n &optional (f 1))
  "Calculate the factorial n (n-1) ... 2 1"
  (if (zerop n)
      f
      (factorial (- n 1)
                 (* n f))))

(defun unrank-test (n)
  (let ((map (amrange 1 n)))
    #1= (let ((r (rank map)))
          (format t "~A .. ~2D .. ~A~%" map r (permutation-unrank n r) ))
    (lambda (i j)
      (rotatef (aref map (1- i))
               (aref map (1- j)))
      #1#)))
