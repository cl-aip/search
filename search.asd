(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  )
(defpackage :search-system
  (:use :common-lisp :asdf))

(in-package :search-system)

(defsystem :search
  :name "Search"
  :author "Seiji Koide"
  :maintainer "Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.0.1"
  :license "MIT"
  :description "Typical algorithms for search in AI"
  :long-description "depth-first-search, breadth-first-search, best-first-search, and A* algorithms"
  :components
  ((:file "search")))