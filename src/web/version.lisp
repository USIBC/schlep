;;; schlep/src/web/version.lisp
;;; D. Racine 20210513

(in-package :schlep)


(defun get-version (id)
  "Read and return app id's version from its About screen"
  (with-form (f *about-path* id)
    (let ((elts (remove-if-not (lambda (x) (string-equal (attribute x "class") "form_items"))
                               (get-elements-by-tag-name (form-dom f) "span"))))
      (if (> (length elts) 0)
        (text (car (last elts)))
        (values nil "Version scrape failed")))))
