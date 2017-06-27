# Cl-Batis - SQL Mapping Framework for Common Lisp

## Usage

### Define SQL

```common-lisp
@select (
         " select
             product_id,
             product_name,
             product_price,
             product_image,
             product_url
           from
             product
           where
             valid_flag = '1' "
           @sq-condition (not (null product_name))
                         " and
                             product_name like :product_name "
           @sql-condition (and (not (null price_low))
                               (not (null price_high)))
                          " and
                              product_price between :price_row and :price_high "
           " order by
               product_id
             limit :limit ")
(defsql fetch-product (product_name price_low price_high limit)"
```

## Databases

* SQLite3
* PostgreSQL
* MySQL

implemented by CL-DBI

## Installation


## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the MIT License.
