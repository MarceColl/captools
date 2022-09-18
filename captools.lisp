(defpackage #:captools
  (:use #:cl #:captools.db #:captools.config #:iterate)
  (:export run-tests-by-prefix))

(in-package #:captools)

(defparameter *num-parallel-executions* 10)
(defparameter *tests* nil)

(setf lparallel:*kernel* (lparallel:make-kernel (getf *config* :num-workers)))

(defun get-n-paths (pathname n)
  (let* ((subpaths (uiop:split-string pathname :separator "/"))
         (n (if (string= (car subpaths) "")
                (+ n 1)
                n)))
    (format nil "~{~A~^/~}" (subseq subpaths 0 n))))

(defun get-config-file-from-test-file (test-file)
  (let ((conf "/jest.config.js")
        (conf-path
          (cond
           ((string= (subseq test-file 0 11) "libs/domain")
            (get-n-paths test-file 3))
           ((string= (subseq test-file 0 5) "libs/")
            (get-n-paths test-file 2))
           ((string= (subseq test-file 0 5) "apps/")
            (get-n-paths test-file 2)))))
    (format nil "~a~a" conf-path conf)))

(defun parse-results (res)
  (iter
    (for r in-vector res)
    (collect `(:ancestors ,(gethash "ancestorTitles" r)
               :title ,(gethash "title" r)
               :messages ,(gethash "failureMessages" r)
               :status ,(gethash "status" r)))))

(defun run-test (test-file)
  (with-pristine-db
    (let ((config-file (get-config-file-from-test-file test-file)))
      (multiple-value-bind (results)
          (exec-in-db-env `("yarn" "--silent" "jest" "-c" ,config-file ,test-file "--forceExit" "--json" "--runInBand" "--passWithNoTests"))
        (let* ((decoded (com.inuoe.jzon:parse results))
               (fts (gethash "numFailedTestSuites" decoded))
               (ft  (gethash "numFailedTests" decoded))
               (pts (gethash "numPassedTestSuites" decoded))
               (pt  (gethash "numPassedTests" decoded))
               (tts (gethash "numTotalTestSuites" decoded))
               (tt  (gethash "numTotalTests" decoded))
               (tr (aref (gethash "testResults" decoded) 0))
               (st (gethash "startTime" tr))
               (et (gethash "endTime" tr))
               (res (gethash "assertionResults" tr)))
          `(:test-file ,test-file
            :test-suites (:passed ,pts :failed ,fts :total ,tts)
            :tests (:passed ,pt :failed ,ft :total ,tt)
            :duration ,(/ (- et st) 1000)
            :results ,(parse-results res)))))))

(defun start-tests (test-list)
  "Starts a test run of all tests given in argument list.
Each test is ran in a pristine database environment in parallel with at most 10 workers.

This will return a list of promises containing the results of the tests."
  (format t "Starting tests...~%")
  (reset-database)
  (setup-template-tables)
  (mapcar
    (lambda (test-file)
      (lparallel:future
        (run-test test-file)))
    test-list))

(defun run-tests-by-prefix (prefix)
  (setf *tests* (start-tests (get-files-by-prefix prefix))))

(defun get-all-files ()
  (multiple-value-bind (files) (exec-in-db-env '("find" "./apps/api" "-name" "*.spec.ts"))
    (mapcar
     (lambda (s)
       (subseq s 2))
     (cl-ppcre:split "\\n" files))))

(defun get-files-by-prefix (prefix)
  (multiple-value-bind (files) (exec-in-db-env `("find" ,prefix "-name" "*.spec.ts"))
    (mapcar
     (lambda (s)
       (subseq s 2))
     (cl-ppcre:split "\\n" files))))

(defun stop-tests ()
  (lparallel:kill-tasks :default))

(defun finished-tests ()
  (remove-if-not #'lparallel:fulfilledp *tests*))

(defun pending-tests ()
  (remove-if #'lparallel:fulfilledp *tests*))

(defun time-since ()
  (- (get-universal-time) *start*))

(defun check-finish ()
  (if (= (length (pending-tests)) 0)
      (time-since)
      nil))

