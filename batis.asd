#|
  This file is a part of CL-BATIS project.
  Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)
|#

#|
  SQL Mapping Framework for Common Lisp

  Author: tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage batis-asd
  (:use :cl :asdf))
(in-package :batis-asd)

(defsystem batis
  :version "0.1"
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:cl-dbi
               :cl-syntax
               :cl-syntax-annot)
  :components ((:module "src"
                :components
                ((:file "macro" :depends-on ("sql"))
                 (:file "sqlparser")
                 (:file "sql"))))
  :description "SQL Mapping Framework for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-batis-test))))
