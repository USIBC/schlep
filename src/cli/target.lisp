;;; schlep/src/cli/target.lisp
;;; D. Racine 20170502

(in-package :schlep)


(defun target-cmd (args)
  "args is a list of strings denoting target's args as specified on the CLI"
  (flet ((arg1-is (s) (string= (car args) s)))
    (cond
      ((arg1-is "list")
       (format t "~4,,,a  ~30,,,a  ~9,,,a  ~a~%" "id" "baseURL" "nodecount" "user")
       (dolist (i *targets*)
         (format t "~4,,,a  ~30,,,a  ~9,,,@a  ~a~%"
                 (target-id i) (target-url i) (target-nodecount i) (target-user i))))
      ((and (arg1-is "add") (> (length args) 5))
       (add-target (second args) (third args)
                   (parse-integer (fourth args)) ;nodecount
                   (fifth args) (sixth args)))
      ((and (arg1-is "remove") (> (length args) 1))
       (if (get-target (second args))
           (remove-target (second args))
           (stderr (s+ "unknown target " (prin1-to-string (second args))) 2)))
      (t (stderr "target: unknown or insufficient arguments" 2)))))


(defhandler "target" #'target-cmd)
