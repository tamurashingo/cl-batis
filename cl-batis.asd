(in-package :cl-user)
(defpackage cl-batis-asd
  (:use :cl :asdf))
(in-package :cl-batis-asd)

(defsystem cl-batis
  :version "0.1"
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:batis))
