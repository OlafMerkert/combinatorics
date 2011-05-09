(asdf:defsystem "ccb"
  :depends-on ("ol-utils")
  :serial t
  :components
  ((:file "package")
   (:file "permutations")
   (:file "permutations-rank")))
