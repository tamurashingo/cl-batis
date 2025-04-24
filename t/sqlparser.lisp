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
             (parse "select column from table")))

  ; named parameter
  (ok (equal '(:SQL "select column from table where id = ?  "
               :ARGS (:ID))
             (parse "select column from table where id = :id")))

  ; string literal
  (ok (equal '(:SQL "update table set column = ':column'"
               :ARGS NIL)
             (parse "update table set column = ':column'")))

  ; update SQL
  (ok (equal '(:SQL "select column from table where valid_flag = ?           and id = ?  "
               :ARGS (:VALID_FLAG :ID))
             (parse "select column from table where valid_flag = :valid_flag and id = :id")))

  ; insert SQL
  (ok (equal '(:SQL "insert into table (id, column, valid_flag) values (?  , ?      , ?          )"
               :ARGS (:ID :COLUMN :VALID_FLAG))
             (parse "insert into table (id, column, valid_flag) values (:id, :column, :valid_flag)")))

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
                  m_user.valid_start_date <= ?          
                and
                  m_user.valid_end_date > ?        
                and
                  m_team.valid_start_date <= ?          
                and
                  m_team.valid_end_date > ?        
                and
                  m_group.valid_start_date <= ?          
                and
                  m_group.valid_end_date > ?        "
          :ARGS (:USER_ID :START_DATE :END_DATE :START_DATE :END_DATE :START_DATE :END_DATE))
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
                  m_user.valid_start_date <= :start_date
                and
                  m_user.valid_end_date > :end_date
                and
                  m_team.valid_start_date <= :start_date
                and
                  m_team.valid_end_date > :end_date
                and
                  m_group.valid_start_date <= :start_date
                and
                  m_group.valid_end_date > :end_date")))

  ; apostrophe in string
  (ok (equal '(:SQL "update table set content = 'Don''t'"
               :ARGS NIL)
             (parse "update table set content = 'Don''t'")))

  ; apostrophe and colon in string
  (ok (equal '(:SQL "update table set content = 'Don''t:id'"
               :ARGS NIL)
             (parse "update table set content = 'Don''t:id'"))))

