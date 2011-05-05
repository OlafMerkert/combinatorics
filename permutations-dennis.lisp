(in-package #:ccb)

(defstruct permutation  n map imap)

(defun empty-permutation (n)
  (make-permutation :n n :map #1= (make-array n :element-type 'fixnum)
                    :imap #1#))

(defun permutation-build-imap (perm)
  (loop for i from 1 to (permutation-n perm) do
       (setf (unpermute perm
                        (permute perm i))
             i))
  perm)

(defun permute (perm i)
  "Apply the permutation PERM at i."
  (aref (permutation-map perm) (1- i)))

(defun unpermute (perm i)
  "Apply the inverse of the permutation PERM at i."
  (aref (permutation-imap perm) (1- i)))

(defsetf permute (perm i) (value)
  `(setf (aref (permutation-map ,perm) (1- ,i)) ,value))

(defsetf unpermute (perm i) (value)
  `(setf (aref (permutation-imap ,perm) (1- ,i)) ,value))

(defun permutation-rank (p)
  (let ((n (permutation-n p))
        (r 0))
    (loop for i from 1 to n do
         (let* ((moves (loop for j from 1 below i
                           when (< (unpermute p j) (unpermute p i))
                           count 1))
               (remainder (if (oddp r)
                              moves
                              (- i 1 moves))))
           (setf r (+ (* i r) remainder))))
    r))

(defun permutation-unrank (n m)
  (let ((perm (empty-permutation n))
        (p m)
        r
        dir
        k
        )
    (loop for j from n downto 1 do
         (setf r (mod p j)
               p (floor p j))
         (if (oddp p)
             (setf k 0
                   dir 1)
             (setf k (+ n 1)
                   dir -1))
         (loop
            with c = 0
            do (incf k dir)
            when (zerop (permute perm k))
            do (incf c)
            until (= c (+ r 1)))
         (setf (permute perm k) j))
    perm))

(defun bounding-factorial-number (k)
  "Calculate the smallest N, s.t. K <= N!"
  (labels ((rec (f n)
             (setf f (* f n))
             (if (<= k f)
                 n
                 (rec f (+ n 1)))))
    (rec 1 1)))

(defun permutations-list (n)
  (let ((perm (empty-permutation n))
        (d    (make-array n))
        (a    (mrange 2 n))
        (done nil))
    (loop for i from 1 to n do
         (setf (permute perm i) i
               (unpermute perm i) i
               (aref d (1- i)) -1))
    (loop
          until done do
         (permutation-print perm)
         (if a
             (let* ((m (apply #'max a))
                    (j (unpermute perm m)))
               (setf (permute perm j) (permute perm
                                               #2=(+ j (aref d (1- m))))
                     (permute perm #2#) m
                     (unpermute perm m) (+ (unpermute perm m) (aref d (1- m)))
                     (unpermute perm (permute perm j)) j)
               (if (< m (permute perm (+ j (* 2 (aref d (1- m))))))
                   (setf #3=(aref d (1- m)) (- #3#)
                         a (delete m a)))
               (setf a (union a (mrange (+ m 1) n))))
             (setf done t)))))

(defun permutation-print (perm)
  (princ "[")
  (loop for s = nil then t
        for i across (permutation-map perm)
     do (if s (princ " "))
        (princ i))
  (princ "]")
  (terpri))

(defun map-print (arr)
  (let ((n (length arr)))
   (princ "[")
   (princ (aref arr 1))
   (loop for i from 2 to (- n 2) do
        (princ " ")
        (princ (aref arr i)))
   (princ "]")
   (terpri)))

(defun permutation-list (n)
  (let ((p+ (amrange 0 (+ n 1)))
        (p- (amrange 0 (+ n 1)))
        (d  (make-array (+ n 2) :initial-element -1))
        (a  (mrange 2 n)))
    (setf (aref p+ 0) (+ n 1))
    (loop do #|(princ "pi = ")|#
             (map-print p+)
         #|(princ "----------")|#
         #|(terpri)|#
         while a do
         (let* ((m (apply #'max a))
                (j (aref p- m)))
           #|(format t "A: ~A~%D: ~A~%" a d)|#
           #|(format t "M: ~A  J: ~A~%" m j)|#
           (setf (aref p+ j)                (aref p+ (+ j (aref d m)))
                 (aref p+ (+ j (aref d m))) m
                 (aref p- m)                (+ (aref p- m) (aref d m))
                 (aref p- (aref p+ j))      j)
           #|(format t "compare to: ~A~%" (aref p+ (+ j (* 2 (aref d m)))))|#
           (when (< m (aref p+ (+ j (* 2 (aref d m)))))
             (setf (aref d m) (- (aref d m))
                   a          (delete m a)))
           (setf a (union a (mrange (+ m 1) n)))))))
