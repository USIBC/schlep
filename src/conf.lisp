;;; schlep/src/conf.lisp
;;; D. Racine 20170503

(in-package :cl-user)

(defpackage :schlep (:use :cl :cl-ppcre :patron :drakma :plump))

(in-package :schlep)


;; General configuration items. Timeout values are in seconds.

(defparameter *concurrency-limit* 6)

(defparameter *login-timeout* 30)

(defparameter *get-timeout* 30)

(defparameter *post-timeout* 1800)

(defparameter *worker-thread-timeout* 1890)

(defparameter *find-new-node-retry-limit* 30) ;only relevant to allgroovy

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
