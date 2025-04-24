(in-package :cl-user)
(defpackage batis-test.macro
  (:use :cl
        :cl-annot
        :batis.macro
        :rove))
(in-package :batis-test.macro)

(cl-syntax:use-syntax :annot)

(setup
  nil)

(teardown
  nil)


;; ----------------------------------------
;; SELECT param 1
;; ----------------------------------------
@select (" select * from product "
         (sql-where
          " valid_flag = '1' "
          (sql-cond (not (null product_name))
                    " and product_name like :product_name ")))
(defsql fetch-product (product_name))

(deftest select-single-param
  (ok (string= " select * from product  where   valid_flag = '1' "
               (funcall fetch-product)))
  (ok (string= " select * from product  where   valid_flag = '1'   and product_name like :product_name "
               (funcall fetch-product :product_name "CommonLisp"))))


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

(deftest select-multi-param
  (ok (string= " select * from product "
               (funcall fetch-product2)))
  (ok (string= " select * from product  where   product_name like :product_name "
               (funcall fetch-product2 :product_name "CommonLisp")))
  (ok (string= " select * from product  where   valid_flag = :valid_flag   and product_price between :product_price_low and :product_price_high "
               (funcall fetch-product2
                        :valid_flag '1'
                        :product_price_low 1000
                        :product_price_high 2000)))
  (ok (string= " select * from product  where   valid_flag = :valid_flag   and product_name like :product_name   and product_price between :product_price_low and :product_price_high "
               (funcall fetch-product2
                        :valid_flag '1'
                        :product_price_low 1000
                        :product_price_high 2000
                        :product_name "CommonLisp"))))


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

(deftest update-multi-param
  (ok (string= " update product  set  update_date = :update_date  where  product_id = :product_id "
               (funcall update-product
                        :product_id 1)))
  (ok (string= " update product  set  update_date = :update_date,  product_name = :product_name  where  product_id = :product_id "
               (funcall update-product
                        :product_id 1
                        :product_name "CLHS")))
  (ok (string= " update product  set  update_date = :update_date,  product_name = :product_name,  product_price = :product_price  where  product_id = :product_id "
               (funcall update-product
                        :product_id 1
                        :product_price 3000
                        :product_name "CLHS"))))

