(in-package :cl-user)
(defpackage batis.sqlparser
  (:use :cl
        :cl-ppcre))
(in-package :batis.sqlparser)

(cl-syntax:use-syntax :annot)

(defun param-char-p (c)
  "Check if character is valid for parameter name"
  (scan "^[a-zA-Z0-9_-]$" (string c)))

@export
(defun parse (sql)
  "generate prepared-type SQL and parameters"
  (let* ((len (length sql))
         (pos (lex-normal sql 0 len '()))
         (s (copy-seq sql)))
    (if (null pos)
        (list :sql s :args '())
        (list :sql s
              :args (loop for (start end) in (nreverse pos)
                          collect (let* ((param (subseq sql (1+ start) end)))
                                    (loop for x from start below (1- end)
                                          do (setf (elt s x) #\Space))
                                    (setf (elt s (1- end)) #\?)
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
               (lex-normal sql (1+ pos) len params))
              (t
               (lex-quote sql (1+ pos) len params))))))

(defun lex-doublequote (sql pos len params)
  (if (>= pos len)
      params
      (let ((c (char sql pos)))
        (cond ((char= c #\")
               (lex-normal sql (1+ pos) len params))
              (t
               (lex-doublequote sql (1+ pos) len params))))))

(defun lex-colon (sql pos len params start)
  (if (>= pos len)
      params
      (let ((c (char sql pos)))
        (cond ((char= c #\:)
               (lex-normal sql (1+ pos) len params))
              ((param-char-p c)
               (lex-param sql (1+ pos) len params start))
              (t
               (lex-normal sql pos len params))))))

(defun lex-param (sql pos len params start)
  (if (>= pos len)
      (push (list start pos)
            params)
      (let ((c (char sql pos)))
        (cond ((param-char-p c)
               (lex-param sql (1+ pos) len params start))
              ((char= c #\:)
               (lex-normal sql pos len (push (list start pos)
                                             params)))
              ((or (char= c #\Space)
                   (char= c #\Tab)
                   (char= c #\Return)
                   (char= c #\Linefeed))
               (lex-normal sql (1+ pos) len (push (list start pos)
                                                  params)))
              ((char= c #\')
               (lex-quote sql (1+ pos) len (push (list start pos)
                                                 params)))
              ((char= c #\")
               (lex-doublequote sql (1+ pos) len (push (list start pos)
                                                       params)))
              (t
               (lex-normal sql (1+ pos) len (push (list start pos)
                                                  params)))))))

