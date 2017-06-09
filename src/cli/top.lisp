;;; schlep/src/cli/top.lisp
;;; D. Racine 20170426

(in-package :schlep)


(defun main (argv)
  (sb-ext:disable-debugger)
  (in-package :schlep)
  (cond
    ((< (length argv) 2) (usage) (sb-ext:exit :code 2))
    (t (initialize-conf) (dispatch (cdr argv)))))
