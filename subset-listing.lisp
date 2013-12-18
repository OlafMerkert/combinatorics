(in-package :ccb)

(use-package :iterate)

;;; list all subsets of {1,...,n}

(defun subset-enumerate (n k selector)
  "For every subset of {1,...,n} with k elements, call `selector' on
  the ordered vector describing the subset. We enumerate the subsets
  in colex order."
  (let ((v (amrange 1 k))
        (max-first (- n k)))
    (labels ((find-next-gap ()
               (iter (for j from -1)
                     (for vj in-vector v)
                     (for vj-1 previous vj)
                     (until (and (not (first-iteration-p))
                                 (> vj (+ vj-1 1))))
                     (finally (return j))))
             (increment (j)
               (incf (svref v j))
               (dotimes (i j)
                 (setf (svref v i) (+ i 1))))
             (rec ()
               (funcall selector v)
               (when (<= (svref v 0) max-first)
                 (increment (find-next-gap))
                 (rec))))
      (rec))))

;; (subset-enumerate 6 3 #'print)

