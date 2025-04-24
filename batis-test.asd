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
               :rove)
  :components ((:module "t"
                :components
                ((:file "macro")
                 (:file "sqlparser")
                 (:file "datasource")
                 (:module "sql"
                  :components
                  ((:file "sqlite3-sql")
                   (:file "mysql-sql")
                   (:file "postgresql-sql")))
                 (:module "transaction"
                  :components
                  ((:file "sqlite3-transaction")
                   (:file "mysql-transaction")
                   (:file "postgresql-transaction")
                  )))))
  :description "Test system for CL-BATIS"
  :perform (test-op (op c)
                    (uiop:symbol-call :rove :run c)))
