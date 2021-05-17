;;; schlep/src/cli/about.lisp
;;; D. Racine 20210513

(in-package :schlep)


(defun about-job (id)
  (cons id (multiple-value-list (get-version id))))


(defun about-cmd (idlist)
  "idlist is the list of target id strings as specified on the CLI"
  (let* ((results (schmeer #'about-job idlist))
         (successes (remove-if-not #'second results))
         (fails (remove-if #'second results)))
    (dolist (i (sort successes #'string-lessp :key #'car))
      (format t "~4,,,a  ~a~%" (first i) (second i)))
    (when fails
      (print-fails-and-quit "get-version" fails 1))))


(defhandler "about" #'about-cmd)
