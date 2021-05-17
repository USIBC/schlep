;;; schlep/src/conf.lisp
;;; D. Racine 20170503

(in-package :cl-user)
(defpackage :schlep (:use :cl :cl-ppcre :patron :drakma :plump))
(in-package :schlep)


;; General configuration items

(defparameter *concurrency-limit* 12)

(defparameter *connect-timeout* 30) ;seconds

(defparameter *worker-thread-timeout* 3600) ;seconds

(defparameter *find-new-node-retry-limit* 30) ;applies only to allgroovy

(defparameter *password-password* "an_arbitrary_string")

(defparameter *rcfile* "~/.schleprc")

(defparameter *mimetypes* '(("texthtml"              . ".html")
                            ("textcsv"               . ".csv")
                            ("textplain"             . ".txt")
                            ("applicationexcel"      . ".xls")
                            ("applicationmsword"     . ".doc")
                            ("applicationzip"        . ".zip")
                            ("applicationpdf"        . ".pdf")
                            ("applicationpostscript" . ".ps")))


;; Login operations

(defparameter *login-path* "/Login")

(defun make-login-postdata (userid passwd)
  `(("password" . ,passwd)
    ("username" . ,userid)
    ("redirect" . "/")
    ("loginButton" . "loginButton")))


;; About operations

(defparameter *about-path* "/About")
(defparameter *version-field-label* "[Vv]ersion")


;; Run Mode operations

(defparameter *appmgmt-path* "/ApplicationManagement")

(defparameter *mode-field-label* "[Rr]un  *[Mm]ode")
(defparameter *am-save-button-label* "Save")
(defparameter *locked-mode-field-value* "LOCKED")
(defparameter *unlocked-mode-field-value* "NORMAL")


;; Script Execution operations

(defparameter *scriptexec-path* "/ScriptExecution")

(defparameter *script-field-label* "Current Script")
(defparameter *infile-field-label* "Target Data File")
(defparameter *se-submit-button-label* "Execute Current Script")
