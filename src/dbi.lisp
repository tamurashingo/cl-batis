(in-package :cl-user)
(defpackage batis.dbi
  (:use :cl)
  (:import-from :batis.datasource
                :<sql-session>))
(in-package :batis.dbi)

(cl-syntax:use-syntax :annot)

@export
(defmethod do-sql ((session <sql-session>) sql &optional params)
  (let ((conn (batis.datasource::connection session)))
    (dbi:do-sql conn sql params)))

