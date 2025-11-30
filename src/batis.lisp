(in-package :cl-user)
(defpackage batis
  (:use :cl
        :cl-annot)
  (:nicknames :cl-batis)
  (:import-from :batis.datasource
                :<sql-session>
                :connection
                :create-sql-session
                :with-transaction
                :commit
                :rollback
                :close-sql-session)
  (:import-from :batis.sqlparser
                :parse)
  (:import-from :batis.sql
                :gen-sql-and-params
                :select-one
                :select-list
                :update-one)
  (:import-from :batis.macro
                :<batis-sql>
                :defsql
                :sql-where
                :sql-set
                :sql-cond
                :select
                :update)
  (:import-from :batis.dbi
                :do-sql)
  (:export :<batis-sql>
           :gen-sql-and-params
           :defsql
           :sql-where
           :sql-set
           :sql-cond
           :select
           :update
           :parse

           ;; deprecated symbols
           :<sql-session>
           :connection
           :create-sql-session
           :with-transaction
           :commit
           :rollback
           :close-sql-session
           :select-one
           :select-list
           :update-one
           :do-sql))
(in-package :batis)

