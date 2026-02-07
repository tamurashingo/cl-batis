(in-package :cl-user)
(defpackage batis-test.sqlparser
  (:use :cl
        :batis.sqlparser
        :rove))
(in-package :batis-test.sqlparser)

(setup
  nil)

(teardown
  nil)

(deftest sqlparser
  ; normal SQL
  (ok (equal '(:SQL "select column from table"
               :ARGS NIL)
             (parse "select column from table"
                    '())))

  ; named parameter
  (ok (equal '(:SQL "select column from table where id = ?"
               :ARGS (1))
             (parse "select column from table where id = :id"
                    '(:id 1))))

  ; string literal
  (ok (equal '(:SQL "update table set column = ':column'"
               :ARGS NIL)
             (parse "update table set column = ':column'"
                    '())))
  ; update SQL
  (ok (equal '(:SQL "select column from table where valid_flag = ? and id = ?"
               :ARGS (1 2))
             (parse "select column from table where valid_flag = :valid_flag and id = :id"
                    '(:id 2 :valid_flag 1))))

  ; insert SQL
  (ok (equal '(:SQL "insert into table (id, column, valid_flag) values (?, ?, ?)"
               :ARGS (1 "test" 0))
             (parse "insert into table (id, column, valid_flag) values (:id, :column, :valid_flag)"
                    '(:id 1 :column "test" :valid_flag 0))))

  ; multi line
  (ok (equal
        '(:SQL "select
                  m_group.group_name,
                  m_team.team_name,
                  m_user.user_name
                from
                  m_user,
                  m_team,
                  m_group
                where
                  m_user.user_id = ?
                and
                  m_team.team_id = m_user.team_id
                and
                  m_group.group_id = m_team.team_id
                and
                  m_user.valid_start_date >= ?
                and
                  m_user.valid_end_date < ?
                and
                  m_team.valid_start_date >= ?
                and
                  m_team.valid_end_date < ?
                and
                  m_group.valid_start_date >= ?
                and
                  m_group.valid_end_date < ?"
          :ARGS (1 "2025-01-01" "2025-12-31" "2025-01-01" "2025-12-31" "2025-01-01" "2025-12-31"))
        (parse "select
                  m_group.group_name,
                  m_team.team_name,
                  m_user.user_name
                from
                  m_user,
                  m_team,
                  m_group
                where
                  m_user.user_id = :user_id
                and
                  m_team.team_id = m_user.team_id
                and
                  m_group.group_id = m_team.team_id
                and
                  m_user.valid_start_date >= :start_date
                and
                  m_user.valid_end_date < :end_date
                and
                  m_team.valid_start_date >= :start_date
                and
                  m_team.valid_end_date < :end_date
                and
                  m_group.valid_start_date >= :start_date
                and
                  m_group.valid_end_date < :end_date"
               '(:user_id 1 :start_date "2025-01-01" :end_date "2025-12-31"))))

  ; apostrophe in string
  (ok (equal '(:SQL "update table set content = 'Don''t'"
               :ARGS NIL)
             (parse "update table set content = 'Don''t'"
                    '())))

  ; apostrophe and colon in string
  (ok (equal '(:SQL "update table set content = 'Don''t:id'"
               :ARGS NIL)
             (parse "update table set content = 'Don''t:id'"
                    '()))))

(deftest in-clause-expansion
  (testing "Basic IN clause with multiple values"
    (ok (equal '(:SQL "SELECT * FROM users WHERE id IN (?, ?, ?)"
                 :ARGS (1 2 3))
               (parse "SELECT * FROM users WHERE id IN (:ids)"
                      '(:ids (1 2 3))))))

  (testing "IN clause with single value"
    (ok (equal '(:SQL "SELECT * FROM users WHERE id IN (?)"
                 :ARGS (42))
               (parse "SELECT * FROM users WHERE id IN (:ids)"
                      '(:ids (42))))))

  (testing "IN clause with NIL (empty list)"
    (ok (equal '(:SQL "SELECT * FROM users WHERE id IN (?)"
                 :ARGS (NIL))
               (parse "SELECT * FROM users WHERE id IN (:ids)"
                      '(:ids nil)))))

  (testing "NOT IN clause with multiple values"
    (ok (equal '(:SQL "SELECT * FROM users WHERE id NOT IN (?, ?)"
                 :ARGS (10 20))
               (parse "SELECT * FROM users WHERE id NOT IN (:ids)"
                      '(:ids (10 20))))))

  (testing "Multiple IN clauses"
    (ok (equal '(:SQL "SELECT * FROM products WHERE id IN (?, ?) AND status IN (?, ?)"
                 :ARGS (1 2 "active" "pending"))
               (parse "SELECT * FROM products WHERE id IN (:ids) AND status IN (:statuses)"
                      '(:ids (1 2) :statuses ("active" "pending"))))))

  (testing "Mixed ATOM and LIST parameters"
    (ok (equal '(:SQL "SELECT * FROM users WHERE name = ? AND id IN (?, ?, ?)"
                 :ARGS ("John" 1 2 3))
               (parse "SELECT * FROM users WHERE name = :name AND id IN (:ids)"
                      '(:name "John" :ids (1 2 3))))))

  (testing "IN clause with string values"
    (ok (equal '(:SQL "SELECT * FROM users WHERE status IN (?, ?, ?)"
                 :ARGS ("active" "pending" "approved"))
               (parse "SELECT * FROM users WHERE status IN (:statuses)"
                      '(:statuses ("active" "pending" "approved"))))))

  (testing "Traditional multiple parameters (backward compatibility)"
    (ok (equal '(:SQL "SELECT * FROM users WHERE id IN (?, ?, ?)"
                 :ARGS (1 2 3))
               (parse "SELECT * FROM users WHERE id IN (:id1, :id2, :id3)"
                      '(:id1 1 :id2 2 :id3 3)))))

  (testing "IN clause with PostgreSQL cast"
    (ok (equal '(:SQL "SELECT * FROM users WHERE id IN (?::INTEGER, ?::INTEGER)"
                 :ARGS (1 2))
               (parse "SELECT * FROM users WHERE id IN (:id1::INTEGER, :id2::INTEGER)"
                      '(:id1 1 :id2 2)))))

  (testing "LIST parameter with spaces"
    (ok (equal '(:SQL "SELECT * FROM users WHERE id IN ( ?, ?, ? )"
                 :ARGS (1 2 3))
               (parse "SELECT * FROM users WHERE id IN ( :ids )"
                      '(:ids (1 2 3))))))

  (testing "Complex query with IN and other conditions"
    (ok (equal '(:SQL "SELECT * FROM orders WHERE user_id = ? AND product_id IN (?, ?, ?) AND created_at > ? AND status NOT IN (?)"
                 :ARGS (100 1 2 3 "2024-01-01" "cancelled"))
               (parse "SELECT * FROM orders WHERE user_id = :user_id AND product_id IN (:product_ids) AND created_at > :start_date AND status NOT IN (:excluded_statuses)"
                      '(:user_id 100 :product_ids (1 2 3) :start_date "2024-01-01" :excluded_statuses ("cancelled"))))))

  (testing "IN clause in subquery"
    (ok (equal '(:SQL "SELECT * FROM users WHERE id IN (SELECT user_id FROM orders WHERE product_id = ?)"
                 :ARGS (42))
               (parse "SELECT * FROM users WHERE id IN (SELECT user_id FROM orders WHERE product_id = :product_id)"
                      '(:product_id 42)))))

  (testing "Empty IN with NIL and non-empty IN"
    (ok (equal '(:SQL "SELECT * FROM products WHERE id IN (?) AND status IN (?, ?)"
                 :ARGS (NIL "active" "pending"))
               (parse "SELECT * FROM products WHERE id IN (:ids) AND status IN (:statuses)"
                      '(:ids nil :statuses ("active" "pending"))))))

  (testing "ATOM parameter in IN clause (should work as single value)"
    (ok (equal '(:SQL "SELECT * FROM users WHERE id IN (?)"
                 :ARGS (3))
               (parse "SELECT * FROM users WHERE id IN (:id)"
                      '(:id 3)))))

  (testing "Mixed ATOM in IN clause with other conditions"
    (ok (equal '(:SQL "SELECT * FROM products WHERE category_id = ? AND id IN (?)"
                 :ARGS (10 5))
               (parse "SELECT * FROM products WHERE category_id = :category_id AND id IN (:product_id)"
                      '(:category_id 10 :product_id 5))))))


