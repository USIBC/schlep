;;; schlep/src/cli/rungroovy.lisp
;;; D. Racine 20170519

(in-package :schlep)


(defun rungroovy-job (id script &key outname infile all)
  (let ((f (if all #'allgroovy #'rungroovy)))
    (cons id (multiple-value-list (funcall f id :script-string script
                                           :outfile-basename outname :infile infile)))))


(defun rungroovy-cmd (args-and-target-ids &key all)
  (let* ((stack (if (> (length args-and-target-ids) 1)
                    (copy-list args-and-target-ids)
                    (stderr "(run|all)groovy: insufficient number of arguments" 2)))
         (scriptfile (pop stack))
         (infile-or-1st-id (pop stack))
         (rest-of-ids stack)
         ids script outname infile results fails)
    (cond          ;determine script & outname based on scriptfile arg
      ((string= scriptfile "-")
       (setq script (slurp-string *standard-input*))
       (unless (plusp (length script))
         (stderr "received null script on standard input, no action attempted" 2))
       (setq outname "piped-in-script"))
      ((probe-file scriptfile)
       (setq script (slurp-string (probe-file scriptfile)))
       (setq outname (basename scriptfile)))
      (t (stderr (s+ "script file '" scriptfile "' not found") 2)))
    (cond            ;determine ids & infile based on infile-or-1st-id
      ((get-target infile-or-1st-id)
       (setq ids (cons infile-or-1st-id rest-of-ids)))
      ((probe-file infile-or-1st-id)
       (setq ids rest-of-ids)
       (setq infile (probe-file infile-or-1st-id)))
      (t (stderr (s+ "'" infile-or-1st-id
                     "' is not a file or known target, no action attempted") 2)))
    (let ((jobfn (if all
                     (lambda (i) (rungroovy-job i script
                                                :outname (s+ outname "-out")
                                                :infile infile :all t))
                     (lambda (i) (rungroovy-job i script
                                                :outname (s+ i "-" outname "-out")
                                                :infile infile)))))
      (setq results (schmeer jobfn ids)))
    (setq fails (remove-if #'second results))
    (when fails (print-fails-and-quit (if all "allgroovy" "rungroovy") fails 1))))
      
