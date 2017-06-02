;;; schlep/src/cli/cli-util.lisp
;;; D. Racine 20170426

(in-package :schlep)


(defun schmeer (jobfn idlist)
  "Validates idlist then calls jobfn on each unique member of idlist via run-job, then
  reconciles run-job's results with its input, re-running jobfn on a subset of ids if
  necessary. Returns a list of jobfn(id) return values. jobfn is expected to return a
  list of the form (id results) given an app id as input."
  (let* ((spec (remove-duplicates idlist :test #'string=))
         (ids (remove-if-not #'get-target spec))
         (bad (set-difference spec ids :test #'string=))
         (*print-pretty* nil)
         (results
          (cond
            ((> (length bad) 0)
             (stderr (s+ "Unknown target(s): "
                         (string-trim "()" (princ-to-string bad))))
             (stderr
              "No action attempted on any targets. Exiting with nonzero status." 2))
            ((< (length ids) 1)
             (stderr "No targets specified, no action taken." 2))
            (t (run-job jobfn ids))))
         recon)
    (loop
       while (setq recon (set-difference ids (mapcar #'car results)))
       do (setq results (append results (run-job jobfn recon))))
    results))


(defun print-fails-and-quit (desc fails exit-status)
  (stderr (s+ desc " failed for:"))
  (dolist (i (sort fails #'string-lessp :key #'car))
    (stderr (s+ "  " (princ-to-string (car i)) "  " (princ-to-string (third i)))))
  (stderr "Exiting with nonzero status" exit-status))
