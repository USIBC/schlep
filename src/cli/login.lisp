;;; schlep/src/cli/login.lisp
;;; D. Racine 20170502

(in-package :schlep)


(defun login-job (id)
  "log-into the specified app id and return (id successp username message)"
  (multiple-value-bind (s u m c) (log-into id)
    (declare (ignore c)) (list id s u m)))


(defun login-cmd (idlist)
  "idlist is the list of target id strings as specified on the CLI"
  (let* ((results (schmeer #'login-job idlist))
         (fails (remove-if #'second results)))
    (when fails
      (dolist (i (sort fails #'string-lessp :key #'car))
        (stderr (format nil "~4,,,a  ~10,,,a  ~a" (first i) (third i) (fourth i))))
      (stderr "at least one login failed, exiting with nonzero status" 1))))
