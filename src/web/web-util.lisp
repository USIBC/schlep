;;; schlep/src/web/web-util.lisp
;;; D. Racine 20170511

(in-package :schlep)


(defstruct form
  "Represents an instance of a specific web form with an active session."
  path appid cookiejar dom inputs)


(defstruct input
  "type = 'field | 'button. When submit is t for type 'button, postform will
  use that button's id and value."
  type label id value submit)


(defun getpage (path id cj)
  "Retrieves, parses, and returns the html document at the specified path as a DOM
  root node, given an app instance id and a cookie-jar containing an active session."
  (let ((u (s+ (target-url (get-target id)) path)))
    (multiple-value-bind (body statcode)
        (http-request u :method :get :cookie-jar cj
                      :connection-timeout *connect-timeout*)
      (when (= statcode 200) (parse body)))))


(defun make-postdata (form-inputs)
  (let ((submit (find t form-inputs :key #'input-submit))
        (fields (remove 'button form-inputs :key #'input-type)))
    (mapcar (lambda (x) (cons (input-id x) (input-value x))) (cons submit fields))))


(defun postform (f &key want-stream extra-parameters)
  "POSTs form struct instance f. If want-stream is nil, postform returns nil if
  the app returns anything other than HTTP 200. Note 200 does NOT indicate that
  the intent of the post operation succeeded. If want-stream is t, postform returns
  3 values: an open flexi-stream, response header alist, and HTTP status code."
  (multiple-value-bind (s statcode headers)
      (http-request (s+ (target-url (get-target (form-appid f))) (form-path f))
                    :method :post
                    :parameters (append (make-postdata (form-inputs f)) extra-parameters)
                    :form-data t
                    :cookie-jar (form-cookiejar f)
                    :want-stream want-stream
                    :connection-timeout *connect-timeout*)
    (if want-stream
        (values s headers statcode)
        (when (= statcode 200) (values statcode s)))))


(defun get-filename-extension (headers)
  (let* ((cd (read-tokens-and-parameters (header-value :content-disposition headers)))
         (ct (multiple-value-list (get-content-type headers)))
         (name (cdr (findlist "filename" (cdr (findlist "attachment" cd))))))
    (cond ((stringp name)
           (or (scan-to-strings "\\.[a-zA-Z0-9_][a-zA-Z0-9_]*$" name) ""))
          (ct
           (or (cdr (findlist (s+ (car ct) (cadr ct)) *mimetypes*)) ""))
          (t ""))))


(defun formfield-elt-value (e)
  (flet ((tag-is (x) (string-equal x (tag-name e))))
    (cond
      ((tag-is "input")
       (or (attribute e "value")
           (if (string-equal (attribute e "checked") "checked") "on" "off")))
      ((tag-is "select")
       (let* ((opts (get-elements-by-tag-name e "option"))
              (selected (find-if
                         (lambda (x) (string-equal (attribute x "selected") "selected"))
                         opts)))
         (if selected (attribute selected "value") "")))
      ((tag-is "textarea")
       (if (text-node-p (first-child e))
           (with-output-to-string (*stream*) (serialize-object (first-child e))) ""))
      (t nil))))


(defun labeled-inputs (d)
  "Returns a list of input struct instances for all labeled form fields
  found in DOM root node d."
  (let* ((label-elts (get-elements-by-tag-name d "label"))
         (lbls (mapcar (lambda (x) (attribute x "title")) label-elts))
         (ids (mapcar (lambda (x) (attribute x "for")) label-elts))
         (field-elts (mapcar (lambda (x) (get-element-by-id d x)) ids))
         (vals (mapcar #'formfield-elt-value field-elts)))
    (mapcar
     (lambda (l i v) (make-input :type 'field :label l :id i :value v))
     lbls ids vals)))


(defun hidden-inputs (d)
  "Returns a list of input struct instances for all hidden form inputs
  found in DOM root node d."
  (let ((hidden-input-elts
         (remove-if-not (lambda (x) (string-equal (attribute x "type") "hidden"))
                        (get-elements-by-tag-name d "input"))))
    (mapcar (lambda (x) (make-input :type 'field
                                    :id (or (attribute x "id") (attribute x "name"))
                                    :value (attribute x "value")))
            hidden-input-elts)))


(defun button-links (d)
  "Returns a list of input struct instances for all <a> tags found in
  DOM root node d with attribute class=submit"
  (let* ((elts (remove-if-not (lambda (x) (string-equal (attribute x "class") "submit"))
                              (get-elements-by-tag-name d "a")))
         (ids (mapcar (lambda (x) (attribute x "id")) elts))
         (lbls (mapcar (lambda (x) (when (text-node-p (first-child x))
                                     (with-output-to-string (*stream*)
                                       (serialize-object (first-child x))))) elts)))
    (mapcar (lambda (l i) (make-input :type 'button :label l :id i :value i)) lbls ids)))


(defun button-inputs (d)
  "Returns a list of input struct instances for all <input> tags found in
  DOM root node d with attribute type=submit"
  (let ((elts (remove-if-not (lambda (x) (string-equal (attribute x "type") "submit"))
                             (get-elements-by-tag-name d "input"))))
    (mapcar (lambda (x) (make-input :type 'button
                                    :label (attribute x "value")
                                    :id (or (attribute x "id") (attribute x "name"))
                                    :value (attribute x "value")))
            elts)))


(defun buttons (d) (append (button-links d) (button-inputs d)))


(defun find-input (input-type label-pattern f)
  "Returns the input in form f of type input-type whose label matches
  label-pattern. input-type = 'field | 'button"
  (find-if (lambda (x) (and (eq input-type (input-type x))
                            (scan label-pattern (input-label x))))
           (form-inputs f)))


(defun get-node-id (f)
  (input-value
   (find "Server Name"
         (labeled-inputs (getpage "/ServerManagement" (form-appid f) (form-cookiejar f)))
         :key #'input-label :test #'string-equal)))


(defmacro with-form ((var path id) &body body)
  "Open a session to app id, get the form at path, and bind var to a form
  struct instance containing the info necessary to manipulate the form."
  (alexandria:once-only (path id (s nil) (session-failure-msg nil) (doc nil))
    `(let ((,var (make-form :path ,path :appid ,id)))
       (setq ,s (multiple-value-list (get-session ,id)))
       (if (car ,s)
           (setf (form-cookiejar ,var) (car ,s))
           (setq ,session-failure-msg (second ,s)))
       (unless ,session-failure-msg (setq ,doc (getpage ,path ,id (form-cookiejar ,var))))
       (cond
         (,session-failure-msg (values nil ,session-failure-msg))
         ((null ,doc) (values nil "getpage failed"))
         (t (setf
             (form-dom ,var) ,doc
             (form-inputs ,var) (append (buttons ,doc)
                                        (labeled-inputs ,doc)
                                        (hidden-inputs ,doc)))
            ,@body)))))
