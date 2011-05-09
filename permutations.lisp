(in-package #:ccb)

(defmacro if* (test then &optional else)
  "If with implicit progn for then and else form."
  `(if ,test
       (progn ,@then)
       (progn ,@else)))


(defun permutation-tp (n transposer)
  "Go through all permutations of N objects by transposing neighboured
objects.  TRANSPOSER should be a symmetric function in 2 number
arguments, the places to transpose.  Places are numbered from 1 to N."
  (if (= n 1)
      nil
      (let ((forward nil))
        (labels
            ;; n-1 transpositions to move the first to the last
            ((move-forward ()
               (loop for k from 2 to n
                  do (funcall transposer (- k 1) k))
               (setf forward nil))
             ;; n-1 transpositions to move the last to the first
             (move-backward ()
               (loop for k from n downto 2
                  do (funcall transposer (- k 1) k))
               (setf forward t))
             (new-transposer (i j)
               ;; FORWARD = T means N is currently in first place.
               (if* forward
                    ((funcall transposer (+ i 1) (+ j 1))
                     (move-forward))
                    ((funcall transposer i j)
                     (move-backward)))))
          (move-backward)
          (permutation-tp (- n 1) #'new-transposer)))))

(defun permutation-tp-fast (n transposer)
  #f
  "Go through all permutations of N objects by transposing neighboured
objects.  TRANSPOSER should be a symmetric function in 2 number
arguments, the places to transpose.  Places are numbered from 1 to N."
  (declare (fixnum n) (function transposer))
  (if (= n 1)
      nil
      (let ((forward nil))
        (declare (boolean forward))
        (labels
            ;; n-1 transpositions to move the first to the last
            ((move-forward ()
               (loop for k fixnum from 2 to n
                  do (funcall transposer (the fixnum (- k 1)) k))
               (setf forward nil))
             ;; n-1 transpositions to move the last to the first
             (move-backward ()
               (loop for k fixnum from n downto 2
                  do (funcall transposer (the fixnum (- k 1)) k))
               (setf forward t))
             (new-transposer (i j)
               (declare (fixnum i j))
               ;; FORWARD = T means N is currently in first place.
               (if* forward
                    ((funcall transposer (the fixnum (+ i 1))
                              (the fixnum (+ j 1)))
                     (move-forward))
                    ((funcall transposer i j)
                     (move-backward)))))
          (declare (inline move-backward move-forward))
          (move-backward)
          (permutation-tp (the fixnum (- n 1)) #'new-transposer)))))

(defun printing-transposer (n)
  (let ((map (amrange 1 n)))
    (format t "~A~%" map)
    (lambda (i j)
      (rotatef (aref map (1- i))
               (aref map (1- j)))
      (format t "~A~%" map))))

(defun array-transposer (array)
  (when (integerp array)
    (setf array (amrange array)))
  (values
   (lambda (i j)
     (rotatef (aref array (1- i))
              (aref array (1- j))))
   array))

(defun neighbour-check (i j)
  (declare (fixnum i j))
  (assert (= (abs (- i j)) 1)))

(defun transposition-printer ()
  (lambda (i j)
    (format t "(~A ~A)~%" i j )))

(defun test-fast (n)
 (let ((a (array-transposer n)))
   (time (permutation-tp-fast n a))))

(defun test-slow (n)
  (let ((a (array-transposer n)))
    (time (permutation-tp n a))))

(defmacro! call-with-counter (&body body)
  `(let* ((,g!counter 1)
          (counter (lambda (&rest ,g!args)
                     (declare (ignore ,g!args))
                     (incf ,g!counter))))
     ,@body
     ,g!counter))