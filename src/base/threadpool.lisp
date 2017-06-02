;;; schlep/src/base/threadpool.lisp
;;; D. Racine 20170510

(in-package :schlep)

(defparameter *max-resubmit* 5)

(defparameter *patron*
  (make-instance 'patron
                 :worker-capacity *concurrency-limit*
                 :job-capacity 128
                 :worker-timeout-duration *worker-thread-timeout*))

(defparameter *patron-lock* (bt:make-recursive-lock "patron-lock"))


(defun safe-submit-job (j)
  (bt:with-recursive-lock-held (*patron-lock*) (submit-job *patron* j)))


;; Workaround for sbcl LP#1153309 - failed AVER in sb-kernel:classoid-typep
;; Rerun jobs whose threads fail via patron:job error-report-function
(defun make-resubmitter ()
  "Returns a closure that submits a job to *patron* up to *max-resubmit* times"
  (let ((c 0))
    (lambda (j)
      (cond ((< c *max-resubmit*) (safe-submit-job j) (incf c))
            (t (princ "Exceeded max-resubmit for job ") (princ j))))))


(defun make-result (&optional (i nil i-sup))
  "Returns a closure that conses its arg to its closed-over list, or
  returns the current list if invoked without an arg."
  (let ((x (when i-sup (list i)))
        (m (make-lock)))
    (lambda (&optional (v nil v-sup))
      (with-lock m (if v-sup (setq x (cons v x)) x)))))


(defun run-job (f idlist)
  "Calls function f on each member of idlist as a set of job submissions to
  thread pool *patron*. Returns a list of f(id) return values."
  (let ((r (make-result)))
    (start-patron *patron*)
    (dolist (i idlist)
      (let ((s (make-resubmitter)))
        (safe-submit-job 
         (make-instance 'job
                        :function (lambda () (funcall f i))
                        :result-report-function (lambda (j) (funcall r (result-of j)))
                        :error-report-function (lambda (j) (funcall s j))))))
    (stop-patron *patron* :wait t)
    (funcall r)))
