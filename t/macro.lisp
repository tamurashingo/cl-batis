(in-package :cl-user)
(defpackage batis-test.macro
  (:use :cl
        :cl-annot
        :batis.macro
        :prove))
(in-package :batis-test.macro)

(cl-syntax:use-syntax :annot)

(plan 11)

;; ----------------------------------------
;; SELECT param 1
;; ----------------------------------------
@select (" select * from product where valid_flag = '1' "
         (sql-condition (not (null product_name))
                        " and product_name like :product_name "))
(defsql fetch-product (product_name))

(is (funcall (getf (gethash 'fetch-product batis.sql:*SQL*) :sql-body))
    " select * from product where valid_flag = '1' ")

(is (funcall (getf (gethash 'fetch-product batis.sql:*SQL*) :sql-body)
             :product_name "CommonLisp")
    " select * from product where valid_flag = '1'  and product_name like :product_name ")

(is (getf (gethash 'fetch-product batis.sql:*SQL*) :sql-type)
    'select)


;; ----------------------------------------
;; SELECT param 3
;; ----------------------------------------
@select (" select * from product where valid_flag = '1' "
         (sql-condition (not (null product_name))
                        " and product_name like :product_name ")
         (sql-condition (and (not (null product_price_low))
                             (not (null product_price_high)))
                        " and product_price between :product_price_low and :product_price_high "))
(defsql fetch-product2 (product_name product_price_low product_price_high))


(is (funcall (getf (gethash 'fetch-product2 batis.sql:*SQL*) :sql-body))
    " select * from product where valid_flag = '1' ")

(is (funcall (getf (gethash 'fetch-product2 batis.sql:*SQL*) :sql-body)
             :product_name "CommonLisp")
    " select * from product where valid_flag = '1'  and product_name like :product_name ")

(is (funcall (getf (gethash 'fetch-product2 batis.sql:*SQL*) :sql-body)
             :product_price_low 1000
             :product_price_high 2000)
    " select * from product where valid_flag = '1'  and product_price between :product_price_low and :product_price_high ")

(is (funcall (getf (gethash 'fetch-product2 batis.sql:*SQL*) :sql-body)
             :product_price_low 1000
             :product_price_high 2000
             :product_name "CommonLisp")
    " select * from product where valid_flag = '1'  and product_name like :product_name  and product_price between :product_price_low and :product_price_high ")


;; ----------------------------------------
;; UPDATE param 3
;; ----------------------------------------

@update (" update product set update_date = :update_date "
         (sql-condition (not (null product_name))
                        " ,product_name = :product_name ")
         (sql-condition (not (null product_price))
                        " ,product_price = :product_price ")
         " where product_id = :product_id ")
(defsql update-product (product_name product_price product_id))


(is (funcall (getf (gethash 'update-product batis.sql:*SQL*) :sql-body)
             :product_id 1)
    " update product set update_date = :update_date  where product_id = :product_id ")

(is (funcall (getf (gethash 'update-product batis.sql:*SQL*) :sql-body)
             :product_id 1
             :product_name "CLHS")
    " update product set update_date = :update_date  ,product_name = :product_name  where product_id = :product_id ")

(is (funcall (getf (gethash 'update-product batis.sql:*SQL*) :sql-body)
             :product_id 1
             :product_price 3000
             :product_name "CLHS")
    " update product set update_date = :update_date  ,product_name = :product_name  ,product_price = :product_price  where product_id = :product_id ")


(is (getf (gethash 'update-product batis.sql:*SQL*) :sql-type)
    'update)

(finalize)
