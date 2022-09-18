(defpackage #:captools.db
  (:use #:cl #:dbi #:captools.config #:iterate)
  (:export #:*database-name*
           #:with-pristine-db
           #:print-current-db
           #:reset-database
           #:setup-template-tables
           #:exec-in-db-env))

(in-package #:captools.db)

(defparameter *database-name* nil)
(defparameter *db-conn* nil)

(defun create-pristine-db ()
  (do-sql *db-conn*
    (format nil "CREATE DATABASE ~s WITH TEMPLATE test_template_~a OWNER prisma;" *database-name* (lparallel:kernel-worker-index))))

(defun delete-pristine-db ()
  (do-sql *db-conn*
    (format nil "DROP DATABASE ~s;" *database-name*)))

(defmacro with-pristine-db (&rest body)
  (let ((rg (gensym)))
    `(let* ((*database-name* (arnesi:random-string 32)))
       (with-connection (*db-conn* :postgres
                                :username (get-config :database :username)
                                :password (get-config :database :password)
                                :port (get-config :database :port)
                                :database-name (get-config :database :database-name))
          (create-pristine-db)
          (let ((,rg (ignore-errors
                        (progn ,@body))))
            (delete-pristine-db)
            ,rg)))))

(defun print-current-db ()
  (format t "CURRENT-DB: ~a~%" *database-name*))

(defun reset-database ()
  (let ((*database-name* "tests_template"))
    (exec-in-db-env '("yarn" "prisma" "migrate" "reset" "--skip-seed" "--skip-generate" "-f"))))

(defun setup-template-tables ()
  (dbi:with-connection (conn :postgres
                             :username (get-config :database :username)
                             :password (get-config :database :password)
                             :port (get-config :database :port)
                             :database-name (get-config :database :database-name))
    (dotimes (i (get-config :num-workers))
      (dbi:do-sql conn
         (format nil "DROP DATABASE IF EXISTS test_template_~a;" i))
      (dbi:do-sql conn
         (format nil "CREATE DATABASE test_template_~a WITH TEMPLATE tests_template OWNER prisma;" i)))))

(defun exec-in-db-env (program)
  (uiop:run-program program
    :ignore-error-status t
    :output :string
    :error-output :string
    :environment (list (format nil "TEST_DATABASE_URL=postgres://prisma:prisma@localhost:5433/~a" *database-name*)
                       (format nil "DATABASE_URL=postgres://prisma:prisma@localhost:5433/~a" *database-name*))
    :directory #P"/home/mcoll/capchase/code/client-monorepo/"))
