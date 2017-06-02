;;; schlep/src/cli/top.lisp
;;; D. Racine 20170426

(in-package :schlep)


(defun dispatch (c)
  "c is a list of strings (command [arguments] [targets]) as specified on the CLI"
  (flet ((cmd-is (s) (string-equal (car c) s)))
    (cond
      ((cmd-is "help") (help-cmd (cadr c)))
      ((cmd-is "target") (target-cmd (cdr c)))
      ((cmd-is "login") (login-cmd (cdr c)))
      ((cmd-is "showmode") (showmode-cmd (cdr c)))
      ((cmd-is "lock") (set-runmode-cmd 'lock (cdr c)))
      ((cmd-is "unlock") (set-runmode-cmd 'unlock (cdr c)))
      ((cmd-is "rungroovy") (rungroovy-cmd (cdr c)))
      ((cmd-is "allgroovy") (rungroovy-cmd (cdr c) :all t))
      (t (stderr (s+ "unknown command " (prin1-to-string (car c))) 2)))))


(defun main (argv)
  (sb-ext:disable-debugger)
  (in-package :schlep)
  (cond
    ((< (length argv) 2) (usage) (sb-ext:exit :code 2))
    (t (initialize-conf) (dispatch (cdr argv)))))
