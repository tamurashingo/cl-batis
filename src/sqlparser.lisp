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
(defun parse (sql params)
  "Generate prepared-type SQL and parameters

  sql: SQL string
  params: runtime parameters (property list)

  Returns:
    :sql - prepared statement SQL
    :args - expanded parameter list"
  (multiple-value-bind (prepared-sql param-list)
      (rebuild-sql sql params)
    (list :sql prepared-sql
          :args param-list)))

(defun rebuild-sql (sql params)
  "Rebuild SQL string with parameter expansion

  Returns: (values new-sql param-list)"
  (let ((result (make-array (length sql)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0))
        (param-list '())
        (len (length sql))
        (pos 0))
    (labels ((append-char (c)
               (vector-push-extend c result))

             (append-string (str)
               (loop for c across str
                     do (append-char c)))

             (find-param-end (start)
               "Find the end position of parameter name"
               (loop for i from start below len
                     while (param-char-p (char sql i))
                     finally (return i)))

             (process-param (param-keyword value)
               "Process parameter based on its type"
               (cond
                 ;; NIL case
                 ((null value)
                  (append-char #\?)
                  (push nil param-list))

                 ;; LIST case (non-NIL)
                 ((listp value)
                  ;; Generate "?, ?, ..." - ~* consumes the parameter without outputting it
                  (append-string (format nil "~{?~*~^, ~}" value))
                  (dolist (v value)
                    (push v param-list)))

                 ;; ATOM case
                 (t
                  (append-char #\?)
                  (push value param-list))))

             (scan-sql ()
               "Scan SQL string and build new SQL"
               (loop while (< pos len)
                     do (let ((c (char sql pos)))
                          (cond
                            ;; Single quote
                            ((char= c #\')
                             (append-char c)
                             (incf pos)
                             (scan-quote))

                            ;; Double quote
                            ((char= c #\")
                             (append-char c)
                             (incf pos)
                             (scan-doublequote))

                            ;; Parameter
                            ((char= c #\:)
                             ;; Check for :: (double colon for PostgreSQL cast)
                             (if (and (< (1+ pos) len)
                                      (char= (char sql (1+ pos)) #\:))
                                 ;; :: case - output as is
                                 (progn
                                   (append-string "::")
                                   (setf pos (+ pos 2)))
                                 ;; : case - parameter
                                 (let* ((param-start (1+ pos))
                                        (param-end (find-param-end param-start)))
                                   (if (> param-end param-start)
                                       ;; Valid parameter found
                                       (let* ((param-name (subseq sql param-start param-end))
                                              (param-keyword (intern (string-upcase param-name) :keyword))
                                              (value (getf params param-keyword)))
                                         (process-param param-keyword value)
                                         (setf pos param-end))
                                       ;; No valid parameter, output : and move on
                                       (progn
                                         (append-char #\:)
                                         (incf pos))))))

                            ;; Normal character
                            (t
                             (append-char c)
                             (incf pos))))))

             (scan-quote ()
               "Scan inside single quote"
               (loop while (< pos len)
                     do (let ((c (char sql pos)))
                          (append-char c)
                          (incf pos)
                          (when (char= c #\')
                            (return)))))

             (scan-doublequote ()
               "Scan inside double quote"
               (loop while (< pos len)
                     do (let ((c (char sql pos)))
                          (append-char c)
                          (incf pos)
                          (when (char= c #\")
                            (return))))))

      ;; Start scanning
      (scan-sql)

      ;; Return results
      (values (coerce result 'string)
              (nreverse param-list)))))

