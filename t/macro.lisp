(in-package :cl-user)
(defpackage batis-test.macro
  (:use :cl
        :cl-annot
        :batis.macro
        :rove)
  (:import-from :batis.sql
                :gen-sql-and-params))
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
          (sql-cond (not (null product-name))
                    " and product_name like :product-name ")))
(defsql fetch-product (product-name))

(deftest select-single-param
  (multiple-value-bind (sql params)
      (gen-sql-and-params fetch-product '())
    (ok (string= " select * from product  where   valid_flag = '1' " sql))
    (ok (equal '() params)))

  (multiple-value-bind (sql params)
      (gen-sql-and-params fetch-product '(:product-name "CommonLisp"))
    (ok (string= " select * from product  where   valid_flag = '1'   and product_name like             ? " sql))
    (ok (equal '("CommonLisp") params))))

;; ----------------------------------------
;; SELECT param 3
;; ----------------------------------------
@select (" select * from product "
         (sql-where
          (sql-cond (not (null valid-flag))
                    " valid_flag = :valid-flag ")
          (sql-cond (not (null product-name))
                    " and product_name like :product-name ")
          (sql-cond (and (not (null product-price-low))
                         (not (null product-price-high)))
                    " and product_price between :product-price-low and :product-price-high ")))
(defsql fetch-product2 (valid-flag product-name product-price-low product-price-high))

(deftest select-multi-param
  (multiple-value-bind (sql params)
      (gen-sql-and-params fetch-product2 '())
    (ok (string= " select * from product " sql))
    (ok (equal '() params)))

  (multiple-value-bind (sql params)
      (gen-sql-and-params fetch-product2 '(:product-name "CommonLisp"))
    (ok (string= " select * from product  where   product_name like             ? " sql))
    (ok (equal '("CommonLisp") params)))

  (multiple-value-bind (sql params)
      (gen-sql-and-params fetch-product2 '(:valid-flag 1
                                           :product-price-low 1000
                                           :product-price-high 2000))
    (ok (string= " select * from product  where   valid_flag =           ?   and product_price between                  ? and                   ? " sql))
    (ok (equal '(1 1000 2000) params)))

  (multiple-value-bind (sql params)
      (gen-sql-and-params fetch-product2 '(:valid-flag 1
                                           :product-price-low 1000
                                           :product-price-high 2000
                                           :product-name "CommonLisp"))
    (ok (string= " select * from product  where   valid_flag =           ?   and product_name like             ?   and product_price between                  ? and                   ? " sql))
    (ok (equal '(1 "CommonLisp" 1000 2000) params))))


;; ----------------------------------------
;; UPDATE param 3
;; ----------------------------------------

@update (" update product "
         (sql-set
          " update_date = :update-date, "
          (sql-cond (not (null product-name))
                    " product_name = :product-name, ")
          (sql-cond (not (null product-price))
                    " product_price = :product-price "))
         (sql-where
          "product_id = :product-id "))
(defsql update-product (update-date product-name product-price product-id))

(deftest update-multi-param
  (multiple-value-bind (sql params)
      (gen-sql-and-params update-product '(:update-date "2025-01-01"
                                           :product-id 1))
    (ok (string= " update product  set  update_date =            ?  where  product_id =           ? " sql))
    (ok (equal '("2025-01-01" 1) params)))

  (multiple-value-bind (sql params)
      (gen-sql-and-params update-product '(:update-date "2025-01-01"
                                           :product-id 1
                                           :product-name "CLHS"))
    (ok (string= " update product  set  update_date =            ?,  product_name =             ?  where  product_id =           ? " sql))
    (ok (equal '("2025-01-01" "CLHS" 1) params)))

  (multiple-value-bind (sql params)
      (gen-sql-and-params update-product '(:update-date "2025-01-01"
                                           :product-id 1
                                           :product-price 3000
                                           :product-name "CLHS"))
    (ok (string= " update product  set  update_date =            ?,  product_name =             ?,  product_price =              ?  where  product_id =           ? " sql))
    (ok (equal '("2025-01-01" "CLHS" 3000 1) params))))


;; ----------------------------------------
;; PostgreSQL :: CAST
;; ----------------------------------------
@select (" select * from product where product_id = :product-id::INTEGER ")
(defsql fetch-product-with-cast (product-id))

(deftest select-with-postgresql-cast
  (multiple-value-bind (sql params)
      (gen-sql-and-params fetch-product-with-cast '(:product-id "123"))
    (ok (string= " select * from product where product_id =           ?::INTEGER " sql))
    (ok (equal '("123") params))))

