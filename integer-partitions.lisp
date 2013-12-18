(in-package :ccb)

(defun integer-partitions-enumerate (n partitioner)
  "Enumerate all integer partitions of n, and call `partitioner' with
  the sequence `parts' (ordered strictly descending) and according
  `multiplicities'."
  (let ((parts #1=(make-array n :fill-pointer 0 :initial-element 0 :element-type 'fixnum))
        (mults #1#))
    (vector-push n parts)
    (vector-push 1 mults)
    (labels ((rec (r)
               (setf (fill-pointer parts) (+ r 1)
                     (fill-pointer mults) (+ r 1))
               (funcall partitioner parts mults)
               (cond
                 ;; if the last part of the partition is not 1, we split it
                 ((< 1 (aref parts r))
                  (split-up-last (aref parts r) r))
                 ;; otherwise, we can only split if another part
                 ;; exists, from which we split -- and we also use the
                 ;; 1 part
                 ((< 0 r)
                  (split-up-last (+ (aref parts (- r 1)) (aref mults r))
                                 (- r 1)))))
             (split-up-last (s k)
               ;; the new part is obtained by decreasing the last
               ;; non-1 part, we start off with highest possible
               ;; multiplicity and drop the remains into 1 part.
               (let ((w (- (aref parts k) 1)))
                 (mvbind (u v) (floor s w)
                   ;; we just split part k off once
                   (decf #2=(aref mults k))
                   ;; but appending strategy varies if we killed it completely
                   (redist-mults u v w (if (zerop #2#)
                                           k (+ k 1))))))
             (redist-mults (u v w k)
               (setf (aref mults k) u
                     (aref parts k) w)
               (if (zerop v)
                   ;; the easy part, no 1 part needed
                   (rec k)
                   ;; account for 1 part
                   (let ((k+1 (+ k 1)))
                     (setf (aref mults k+1) 1
                           (aref parts k+1) v)
                     (rec k+1)))))
      (rec 0))))

;; (integer-partitions-enumerate 7 (lambda (p m) (print (map 'list #'list p m))))
