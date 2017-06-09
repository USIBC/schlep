;;; schlep/src/cli/exec.lisp
;;; D. Racine 20170609

(in-package :schlep)


(defun exec-cmd (args)
  (let ((lispfile (car args)))
    (defparameter *exec-args* (cdr args))
    (cond
      ((null lispfile) (stderr "exec: insufficient number of arguments" 2))
      ((string= lispfile "-") (load *standard-input*))
      ((probe-file lispfile) (load (probe-file lispfile)))
      (t (stderr (s+ "file '" lispfile "' not found") 2)))))


(defhandler "exec" #'exec-cmd)
