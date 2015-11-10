(in-package #:cl-user)

(defpackage :constructive-combinatorics
  (:nicknames :ccb)
  (:use #:cl #:ol-utils)
  (:export
   #:permutation-rank
   #:permutation-unrank
   #:factorial
   #:permutation-tp
   #:array-transposer))
