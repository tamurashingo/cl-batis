# Cl-Batis - SQL Mapping Framework for Common Lisp

[![Build Status](https://travis-ci.org/tamurashingo/cl-batis.svg?branch=develop)](https://travis-ci.org/tamurashingo/cl-batis)


## Usage

### create session

```common-lisp
;; with CL-DBI connection
(defparameter *conn-dbi* (dbi:connect :mysql
                                      :database-name "batis"
                                      :username "nobody"
                                      :password "nobody"))
(defparameter *session* (create-sql-session *conn-dbi*))


;; with CL-DBI-Connection-Pool
(defparameter *conn-pool* (dbi-cp:make-dbi-connection-pool :mysql
                                                           :database-name "batis"
                                                           :username "nobody"
                                                           :password "nobody"))
(defparameter *session* (create-sql-session *conn-pool*))

;; direct
(defparameter *session* (create-sql-session :mysql
                                            :database-name "batis"
                                            :username "nobody"
                                            :password "nobody"))

```

### Define SQL

There are two type of methods.

- `@update`
- `@select`

Cl-Batis does not support DDL.
If you want to use DDL, use `do-sql`.

```common-lisp
(do-sql session "truncate table product")
```

#### update

```common-lisp
@update ("insert into product (id, name, price) values (:id, :name, :price)")
(defsql register-product (id name price))

@update ("update
            product "
         (sql-set
          (sql-cond (not (null name))
                    " name = :name, ")
          (sql-cond (not (null price))
                    " price = :price "))
         (sql-where
          " id = :id "))
(defsql update-product (id name price))
```

#### select

```common-lisp
@select ("select name, price from product where id = :id")
(defsql search-product (id))

@select ("select id, name, price from product"
         (sql-where
           (sql-cond (not (null name))
                     " and name = :name ")
           (sql-cond (not (null price_low))
                     " and price >= :price_low ")
           (sql-cond (not (null price_high))
                     " and price <= :price_high "))
         " order by id ")
(defsql filter-product (name price_low price_high))
```

#### where, set

```common-lisp
@select
("select * from product where "
 (sql-cond (not (null price))
           " price = :price")
 (sql-cond (not (null valid_flag))
           " and valid_flag = :valid_flag"))
(defsql search-by-price (price valid_flag))
```

In dynamic condition, if `sql-cond` returns nothing, you would end up with SQL that looked like this:

```SQL
select * from product where
```

This would fail.
And, if only the second condition was met, you would end up with SQL that looked like this:

```SQL
select * from product where
and valid_flag = '1'
```

This would also fail.


So, `cl-batis` provides `SQL-WHERE` function.

```common-lisp
@select
("select * from product"
 (sql-where
   (sql-cond (not (null price))
    " price = :price")
   (sql-cond (not (null valid_flag))
    " and valid_flag = :valid_flag ")))
(defsql search-by-product (price valid_flag))
```

The `SQL-WHERE` knows to only insert `WHERE` if there is any condition.
Furthermore, if that content begins with `AND` or `OR`, strip it off.


```common-lisp
@update
("update product"
 (sql-set
  (sql-cond (not (null price))
            " price = :price, ")
  (sql-cond (not (null name))
            " name = :name "))
 (sql-where
  " id = :id "))
(defsql update-product-info (id price name))
```

There is a similar solution for dynamic update statements called `SQL-SET`.
The `SQL-SET` knows to strip last comma off.



### Execute

#### update

```common-lisp
(update-one *session* register-product :id 1 :name "NES" :price 14800)
```

#### select

```common-lisp
(select-one *session* search-product :id 1)
  -> (:|name| "NES" :|price| 14800))
```

```common-lisp
(select-list *session* filter-product :price_low 20000)
  ->((:|id| 2 :|name| "SNES" :|price| 25000)
     (:|id| 3 :|name| "MEGA DRIVE" :|price| 21000)
     (:|id| 4 :|name| "PC Engine" :|price| 24800)))
```

### transaction

```common-lisp
(commit *session*)

(rollback *session*)
```

### release session

```common-lisp
(close-sql-session *session*)
```

## Databases

* SQLite3
* PostgreSQL
* MySQL


## Installation

This library will be available on Quicklisp when ready for use.

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the MIT License.
