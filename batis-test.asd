#|
  This file is a part of CL-BATIS project.
  Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage batis-test-asd
  (:use :cl :asdf))
(in-package :batis-test-asd)

(defsystem batis-test
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:batis
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "macro")
                 (:test-file "sqlparser")
                 (:test-file "datasource")
                 (:test-file "sql")
                 (:test-file "transaction"))))
  :description "Test system for CL-BATIS"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
