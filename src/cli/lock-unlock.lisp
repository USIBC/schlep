;;; schlep/src/cli/lock-unlock.lisp
;;; D. Racine 20170517

(in-package :schlep)


(defun showmode-job (id)
  (cons id (multiple-value-list (get-runmode id))))


(defun showmode-cmd (idlist)
  "idlist is the list of target id strings as specified on the CLI"
  (let* ((results (schmeer #'showmode-job idlist))
         (successes (remove-if-not #'second results))
         (fails (remove-if #'second results)))
    (dolist (i (sort successes #'string-lessp :key #'car))
      (format t "~4,,,a  ~a~%" (first i) (second i)))
    (when fails
      (print-fails-and-quit "get-runmode" fails 1))))


(defun set-runmode-job (action id)
  (cons id (multiple-value-list (set-runmode action id))))


(defun set-runmode-cmd (action idlist)
  "idlist is the list of target id strings as specified on the CLI
  action = 'lock | 'unlock"
  (let* ((results (schmeer (lambda (x) (set-runmode-job action x)) idlist))
         (fails (remove-if #'second results)))
    (when fails
      (print-fails-and-quit (s+ (string action) " action") fails 1))))
