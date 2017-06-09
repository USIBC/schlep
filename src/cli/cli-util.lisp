;;; schlep/src/cli/cli-util.lisp
;;; D. Racine 20170426

(in-package :schlep)


(defparameter *cmd-dispatch-table* (make-hash-table :test 'equal)
  "command:handler; 'command' is a string and 'handler' is a function of one arg,
  expected to be a list of strings representing the args and targets specified after
  the command on the CLI.")


(defun defhandler (command handler-fn)
  (setf (gethash command *cmd-dispatch-table*) handler-fn))


(defun dispatch (c)
  "c is a list of strings (command [arguments] [targets]) as specified on the CLI"
  (let ((handler (gethash (car c) *cmd-dispatch-table*)))
    (if handler
        (funcall handler (cdr c))
        (stderr (s+ "unknown command " (prin1-to-string (car c))) 2))))


(defun schmeer (jobfn appids)
  "Validates appids then calls jobfn on each unique member of appids via run-job, then
  reconciles run-job's results with its input, re-running jobfn on a subset of ids if
  necessary. Returns a list of jobfn(id) return values. jobfn is expected to return a
  list of the form (id results) given an app id as input."
  (let* ((spec (remove-duplicates appids :test #'string=))
         (ids (remove-if-not #'get-target spec))
         (bad (set-difference spec ids :test #'string=))
         (results (if (zerop (length ids))
                      (stderr "No known targets specified, no action taken." 2)
                      (run-job jobfn ids)))
         recon)
    (loop
       while (setq recon (set-difference ids (mapcar #'car results)))
       do (setq results (append results (run-job jobfn recon))))
    (append results (mapcar (lambda (x) (list x nil "unknown target")) bad))))


(defun print-fails-and-quit (desc fails exit-status)
  (stderr (s+ desc " failed for:"))
  (dolist (i (sort fails #'string-lessp :key #'car))
    (stderr (s+ "  " (format nil "~4,,,a" (car i)) "  "
                (reduce (lambda (x y) (s+ x "  " y)) (or (nthcdr 2 i) '(""))))))
  (stderr "Exiting with nonzero status" exit-status))
