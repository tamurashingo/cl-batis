(in-package :cl-user)
(defpackage batis.sqlparser
  (:use :cl))
(in-package :batis.sqlparser)

(cl-syntax:use-syntax :annot)

@export
(defun parse (sql)
  "generate prepared-type SQL and parameters"
  (let* ((len (length sql))
         (pos (lex-normal sql 0 len '()))
         (s (copy-seq sql)))
    (if (null pos)
        (list :sql s :args '())
        (list :sql s
              :args (loop for position in pos
                          collect (let* ((start (getf position :start))
                                         (end (getf position :end))
                                         (param (subseq sql (1+ start) end)))
                                    (setf (elt s start) #\?)
                                    (loop for x from (1+ start) below end
                                          do (setf (elt s x) #\Space))
                                    (intern (string-upcase param) :keyword)))))))

(defun lex-normal (sql pos len params)
  (if (>= pos len)
      params
      (let ((c (char sql pos)))
        (cond ((char= c #\')
               (lex-quote sql (1+ pos) len params))
              ((char= c #\")
               (lex-doublequote sql (1+ pos) len params))
              ((char= c #\:)
               (lex-colon sql (1+ pos) len params pos))
              (t
               (lex-normal sql (1+ pos) len params))))))

(defun lex-quote (sql pos len params)
  (if (>= pos len)
      params
      (let ((c (char sql pos)))
        (cond ((char= c #\')
               (lex-quote1 sql (1+ pos) len params))
              (t
               (lex-quote sql (1+ pos) len params))))))

(defun lex-quote1 (sql pos len params)
  (if (>= pos len)
      params
      (let ((c (char sql pos)))
        (cond ((char= c #\')
               (lex-quote sql (1+ pos) len params))
              ((char= c #\")
               (lex-doublequote sql (1+ pos) len params))
              ((char= c #\:)
               (lex-colon sql (1+ pos) len params pos))
              (t
               (lex-normal sql (1+ pos) len params))))))

(defun lex-doublequote (sql pos len params)
  (if (>= pos len)
      params
      (let ((c (char sql pos)))
        (cond ((char= c #\")
               (lex-doublequote1 sql (1+ pos) len params))
              (t
               (lex-doublequote sql (1+ pos) len params))))))

(defun lex-doublequote1 (sql pos len params)
  (if (>= pos len)
      params
      (let ((c (char sql pos)))
        (cond ((char= c #\")
               (lex-doublequote sql (1+ pos) len params))
              ((char= c #\')
               (lex-quote sql (1+ pos) len params))
              ((char= c #\:)
               (lex-colon sql (1+ pos) len params pos))
              (t
               (lex-normal sql (1+ pos) len params))))))

(defun lex-colon (sql pos len params start)
  (if (>= pos len)
      (nreverse (push (list :start start :end pos)
                      params))
      (let ((c (char sql pos)))
        (cond ((find c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
               (lex-colon sql (1+ pos) len params start))
              ((or (char= c #\Space)
                   (char= c #\Tab)
                   (char= c #\Return)
                   (char= c #\Linefeed))
               (lex-normal sql (1+ pos) len (push (list :start start :end pos)
                                                  params)))
              ((char= c #\')
               (lex-quote sql (1+ pos) len (push (list :start start :end pos)
                                                 params)))
              ((char= c #\")
               (lex-quote sql (1+ pos) len (push (list :start start :end pos)
                                                 params)))
              ((char= c #\:)
               (lex-colon sql (1+ pos) len (push (list :start start :end pos)
                                                 params) pos))
              (t
               (lex-normal sql (1+ pos) len (push (list :start start :end pos)
                                                  params)))))))

