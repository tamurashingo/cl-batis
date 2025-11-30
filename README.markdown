# Cl-Batis - SQL Mapping Framework for Common Lisp

![ci workflow](https://github.com/tamurashingo/cl-batis/actions/workflows/ci.yml/badge.svg)

## Overview

Cl-Batis is a library for generating prepared statement queries and their parameters.
This library focuses on SQL definition and generation, delegating query execution to other libraries.

## Usage

### Define SQL

There are two types of methods for defining SQL:

- `update` - for INSERT, UPDATE, DELETE statements
- `select` - for SELECT statements

When using `(cl-syntax:use-syntax :annot)`, `@update` and `@select` annotations can be used.

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

### Generate SQL and Parameters

Use `gen-sql-and-params` to generate the prepared statement SQL and its parameters:

```common-lisp
(gen-sql-and-params register-product :id 1 :name "NES" :price 14800)
; => "insert into product (id, name, price) values (?, ?, ?)"
;    (1 "NES" 14800)

(gen-sql-and-params filter-product :name nil :price_low 20000 :price_high nil)
; => "select id, name, price from product WHERE price >= ? order by id "
;    (20000)
```

### Dynamic Conditions

#### where, set

In dynamic conditions, if `sql-cond` returns nothing, you would end up with SQL that looked like this:

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


So, `cl-batis` provides `sql-where` function.

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

The `sql-where` knows to only insert `WHERE` if there is any condition.
Furthermore, if that content begins with `AND` or `OR`, it strips it off.


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

There is a similar solution for dynamic update statements called `sql-set`.
The `sql-set` knows to strip the last comma off.

## Installation

~~This library is available on Quicklisp.~~

use qlot.


## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2017, 2024, 2025 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the MIT License.

---

## Deprecated Features (for backward compatibility)

The following features are deprecated and will be removed in a future version.
These features were related to session management and SQL execution, which are now delegated to other libraries.

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

### how to do DDL

Cl-Batis does not support DDL.
If you want to use DDL, use `do-sql`.

```common-lisp
(do-sql session "truncate table product")
```

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


When exiting the `transaction-macro` block, it will automatically commit.

```common-lisp
(with-transaction *session*
  ; blah blah blah
)
```


To explicitly commit, use `commit`.

```common-lisp
(with-transaction *session*
  (update-one *session* register-product :id 1 :name "NES" :price 14800)
  (commit *session*))
```

You can roll back using `rollback`.

```common-lisp
(with-transaction *session*
  ;blah
  ;blah
  ;blah
  (rollback *session*))
```

### release session

```common-lisp
(close-sql-session *session*)
```

### Databases

* SQLite3
* PostgreSQL
* MySQL

### Example

```common-lisp
;;;
;;; create session
;;;
CL-USER> (defparameter session
           (create-sql-session :mysql
                               :database-name "scdata"
                               :username "root"
                               :password "password"))
SESSION

;;;
;;; create table
;;;
CL-USER> (do-sql session "create table product (id integer primary key, name varchar(20) not null, price integer not null)")
; No value

;;;
;;; define sql
;;;
CL-USER> (select (" select * from product where id = :id ")
                 (defsql select-product (id)))
SELECT-PRODUCT
CL-USER> (select (" select name, price from product "
                  (sql-where
                   (sql-cond (not (null name))
                             " and name = :name ")
                   (sql-cond (not (null price_low))
                             " and price >= :price_low ")
                   (sql-cond (not (null price_high))
                             " and price <= :price_high "))
                  " order by id ")
                 (defsql select-product-by-name-or-price (name price_low price_high)))
; in:
;      SELECT (" select name, price from product "
;          (SQL-WHERE (SQL-COND (NOT (NULL NAME)) " and name = :name ")
;                     (SQL-COND (NOT (NULL PRICE_LOW))
;                               " and price >= :price_low ")
;                     (SQL-COND (NOT (NULL PRICE_HIGH))
;                               " and price <= :price_high "))
;          " order by id ")
;     (NULL NAME)
; --> IF
; ==>
;   NAME
;
; caught STYLE-WARNING:
;   reading an ignored variable: NAME

;     (NULL PRICE_LOW)
; --> IF
; ==>
;   PRICE_LOW
;
; caught STYLE-WARNING:
;   reading an ignored variable: PRICE_LOW

;     (NULL PRICE_HIGH)
; --> IF
; ==>
;   PRICE_HIGH
;
; caught STYLE-WARNING:
;   reading an ignored variable: PRICE_HIGH
;
; compilation unit finished
;   caught 3 STYLE-WARNING conditions
SELECT-PRODUCT-BY-NAME-OR-PRICE
CL-USER> (update ("insert into product (id, name, price) values (:id, :name, :price)")
                 (defsql register-product (id name price)))
REGISTER-PRODUCT

;;;
;;; insert
;;;
CL-USER> (update-one session register-product :id 1 :name "NES" :price 14800)
(1)
CL-USER> (update-one session register-product :id 2 :name "SNES" :price 25000)
(1)
CL-USER> (update-one session register-product :id 3 :name "MEGA DRIVE" :price 21000)
(1)
CL-USER> (update-one session register-product :id 4 :name "PC Engine" :price 24800)
(1)

;;;
;;; select one record
;;;
CL-USER> (select-one session select-product :id 1)
(:|id| 1 :|name| "NES" :|price| 14800)

;;;
;;; select some records
;;;
CL-USER> (select-list session select-product-by-name-or-price)
((:|name| "NES" :|price| 14800) (:|name| "SNES" :|price| 25000)
 (:|name| "MEGA DRIVE" :|price| 21000) (:|name| "PC Engine" :|price| 24800))
CL-USER> (select-list session select-product-by-name-or-price :price_low 20000)
((:|name| "SNES" :|price| 25000) (:|name| "MEGA DRIVE" :|price| 21000)
 (:|name| "PC Engine" :|price| 24800))
CL-USER> (select-list session select-product-by-name-or-price :price_low 20000 :price_high 22000)
((:|name| "MEGA DRIVE" :|price| 21000))
CL-USER> (select-list session select-product-by-name-or-price :name "PC Engine")
((:|name| "PC Engine" :|price| 24800))
```
