;;; schlep/src/base/rc.lisp -- Persistence of local config in ~/.schleprc
;;; D. Racine 20170425

(in-package :schlep)

(defparameter *targets* nil)

(defstruct target id url nodecount user pass)


(defun initialize-conf ()
  (let* ((rc (probe-file *rcfile*))
         (conf (if rc
                   (with-open-file (s rc :direction :input) (read s nil))
                   (with-open-file (s *rcfile* :direction :output :if-exists :supersede)
                     (prin1 nil s)))))
    (setq *targets* conf)))


(defun add-target (id url nodecount user pass)
  "Encrypts password and adds an app instance to rcfile"
  (when (find id *targets* :key #'target-id :test #'string=)
    (stderr "target not added: a target with that ID already exists" 2))
  (setq *targets*
        (sort (cons (make-target :id id :url url :nodecount nodecount :user user
                                 :pass (encrypt pass id *password-password*))
                    *targets*)
              #'string-lessp :key #'target-id))
  (with-open-file (s *rcfile* :direction :output :if-exists :supersede)
    (prin1 *targets* s)))


(defun get-target (id)
  "Returns a target with decrypred password"
  (let ((x (find id *targets* :key #'target-id :test #'string=)))
    (when x (make-target :id (target-id x) :url (target-url x)
                         :nodecount (target-nodecount x) :user (target-user x)
                         :pass (decrypt (target-pass x) id *password-password*)))))


(defun remove-target (id)
  (unless (get-target id) (stderr "target not found, no action taken" 2))
  (setq *targets* (remove id *targets* :key #'target-id :test #'string=))
  (with-open-file (s *rcfile* :direction :output :if-exists :supersede)
    (prin1 *targets* s)))
