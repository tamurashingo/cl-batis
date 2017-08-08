(in-package :cl-user)
(defpackage batis
  (:use :cl
        :cl-annot)
  (:nicknames :cl-batis)
  (:import-from :batis.datasource
                :<sql-session>
                :connection
                :create-sql-session
                :close-sql-session)
  (:import-from :batis.sqlparser
                :parse)
  (:import-from :batis.sql
                :select-one
                :select-list
                :update-one)
  (:import-from :batis.macro
                :defsql
                :sql-condition
                :select
                :update)
  (:export :<sql-session>
           :connection
           :create-sql-session
           :close-sql-session
           :parse
           :select-one
           :select-list
           :update-one
           :defsql
           :sql-condition
           :select
           :update))
(in-package :batis)

