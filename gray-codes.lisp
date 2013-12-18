(in-package :ccb)

(defun coeff-bounds-intervals (coeff-bounds)
  (let* ((l (length coeff-bounds))
         (b-from (make-array (+ 1 l) :initial-element nil)))
    (iter (for b in-vector coeff-bounds with-index i downto 0)
          (setf (aref b-from i)
                (if (< 1 b)
                    (cons i #2=(aref b-from (+ i 1)))
                    #2#)))
    b-from))

(defun product-space-enumerate (coeff-bounds selector)
  (let* ((n (length coeff-bounds))
         (b-from (coeff-bounds-intervals coeff-bounds))
         (a (aref b-from 0))
         (v (make-array n :initial-element 0 :element-type 'fixnum))
         (d (make-array n :initial-element 1 :element-type 'fixnum)))
    (labels ((rec ()
               (funcall selector v)
               (when a
                 (alter-vector (apply #'max a))))
             (alter-vector (p)
               (incf #1=(aref v p) #2=(aref d p))
               (if (or (zerop #1#) (= #1# (- (aref coeff-bounds p) 1)))
                   (setf #2# (- #2#)
                         a (remove p a)))
               ;; todo can we ensure that a is always monotonous descending?
               (setf a (union a (aref b-from (+ p 1))))
               (rec)))
      (rec))))

;; (product-space-enumerate #(2 2 2) #'print)

