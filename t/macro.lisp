(in-package :cl-user)
(defpackage batis-test.macro
  (:use :cl
        :cl-annot
        :batis.macro
        :prove))
(in-package :batis-test.macro)

(cl-syntax:use-syntax :annot)

(plan 9)

;; ----------------------------------------
;; SELECT param 1
;; ----------------------------------------
@select (" select * from product "
         (sql-where
          " valid_flag = '1' "
          (sql-cond (not (null product_name))
                    " and product_name like :product_name ")))
(defsql fetch-product (product_name))

(is (funcall fetch-product)
    " select * from product  where   valid_flag = '1' ")

(is (funcall fetch-product :product_name "CommonLisp")
    " select * from product  where   valid_flag = '1'   and product_name like :product_name ")


;; ----------------------------------------
;; SELECT param 3
;; ----------------------------------------
@select (" select * from product "
         (sql-where
          (sql-cond (not (null valid_flag))
                    " valid_flag = :valid_flag ")
          (sql-cond (not (null product_name))
                    " and product_name like :product_name ")
          (sql-cond (and (not (null product_price_low))
                         (not (null product_price_high)))
                    " and product_price between :product_price_low and :product_price_high ")))
(defsql fetch-product2 (valid_flag product_name product_price_low product_price_high))


;; remove where clause
(is (funcall fetch-product2)
    " select * from product ")

;; remove first `and`
(is (funcall fetch-product2
             :product_name "CommonLisp")
    " select * from product  where   product_name like :product_name ")

(is (funcall fetch-product2
             :valid_flag '1'
             :product_price_low 1000
             :product_price_high 2000)
    " select * from product  where   valid_flag = :valid_flag   and product_price between :product_price_low and :product_price_high ")

(is (funcall fetch-product2
             :valid_flag '1'
             :product_price_low 1000
             :product_price_high 2000
             :product_name "CommonLisp")
    " select * from product  where   valid_flag = :valid_flag   and product_name like :product_name   and product_price between :product_price_low and :product_price_high ")


;; ----------------------------------------
;; UPDATE param 3
;; ----------------------------------------

@update (" update product "
         (sql-set
          " update_date = :update_date, "
          (sql-cond (not (null product_name))
                    " product_name = :product_name, ")
          (sql-cond (not (null product_price))
                    " product_price = :product_price "))
         (sql-where
          "product_id = :product_id "))
(defsql update-product (product_name product_price product_id))


(is (funcall update-product
             :product_id 1)
    " update product  set  update_date = :update_date  where  product_id = :product_id ")

;; remove "product_name = :product_name,"'s last comma
(is (funcall update-product
             :product_id 1
             :product_name "CLHS")
    " update product  set  update_date = :update_date,  product_name = :product_name  where  product_id = :product_id ")

(is (funcall update-product
             :product_id 1
             :product_price 3000
             :product_name "CLHS")
    " update product  set  update_date = :update_date,  product_name = :product_name,  product_price = :product_price  where  product_id = :product_id ")


(finalize)

