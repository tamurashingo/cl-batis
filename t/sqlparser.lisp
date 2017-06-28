(in-package :cl-user)
(defpackage batis-test.sqlparser
  (:use :cl
        :batis.sqlparser
        :prove))
(in-package :batis-test.sqlparser)

(plan 5)

(is (parse "select column from table")
    '(:SQL "select column from table"
      :ARGS NIL))

(is (parse "select column from table where id = :id")
    '(:SQL "select column from table where id = ?  "
      :ARGS (:ID)))

(is (parse "update table set column = ':column'")
    '(:SQL "update table set column = ':column'"
      :ARGS NIL))

(is (parse "select column from table where valid_flag = :valid_flag and id = :id")
    '(:SQL "select column from table where valid_flag = ?           and id = ?  "
      :ARGS (:VALID_FLAG :ID)))

(is (parse
     "select
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
        m_group.valid_end_date > :end_date")
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
       :ARGS (:USER_ID :START_DATE :END_DATE :START_DATE :END_DATE :START_DATE :END_DATE)))

(finalize)
