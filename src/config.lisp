(defpackage #:captools.config
  (:use #:cl)
  (:export #:*config*
           #:get-config
           #:load-config))

(in-package #:captools.config)

(defparameter *config* nil)

(defun load-config (&optional (filepath "config.lisp"))
  (with-open-file (c filepath)
    (setf *config* (read c))))

(defun get-db-config (prop)
  (getf (getf *config* :database) prop))

(defun get-config-inner (keys)
  (let ((curr-key (car keys))
        (next-keys (cdr keys)))
    (if curr-key
        `(getf ,(get-config-inner next-keys) ,curr-key)
        `*config*)))

(defmacro get-config (&rest keys)
  (let ((rkeys (reverse keys)))
    (get-config-inner rkeys)))
