;;; schlep/src/web/runmode.lisp
;;; D. Racine 20170516

(in-package :schlep)


(defun get-runmode (id)
  "Read and return app id's current run mode."
  (with-form (f *appmgmt-path* id)
    (let ((i (find-input 'field *mode-field-label* f)))
      (if i (input-value i) (values nil "Run Mode scrape failed")))))


(defun set-runmode (action id)
  "Sets then confirms Run Mode of app instance id. action = 'lock | 'unlock
  Returns T on success, otherwise returns values NIL and failure message."
  (let ((target-mode (case action
                       (lock *locked-mode-field-value*)
                       (unlock *unlocked-mode-field-value*)
                       (otherwise (bail set-runmode "invalid action")))))
    (with-form (f *appmgmt-path* id)
      (unless (find-input 'field *mode-field-label* f)
        (bail set-runmode "Run Mode scrape failed"))
      (unless (find-input 'button *am-save-button-label* f)
        (bail set-runmode "Save button scrape failed"))
      (setf (input-value (find-input 'field *mode-field-label* f)) target-mode)
      (setf (input-submit (find-input 'button *am-save-button-label* f)) t)
      (or (postform f) (bail set-runmode "postform failed"))
      (sleep 1) ;confirm app actually enters the runmode we just posted
      (let ((count 0) (limit 9) m)
        (loop until (or (and (setq m (get-runmode id)) (string-equal m target-mode))
                        (> count limit))
           do (sleep 1) (incf count))
        (if (<= count limit) t (values nil "mode confirmation failed"))))))
