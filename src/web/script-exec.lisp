;;; schlep/src/web/script-exec.lisp
;;; D. Racine 20170518

(in-package :schlep)


(defun populate-se-form (f script-string infile)
  (unless (find-input 'button *se-submit-button-label* f)
    (bail populate-se-form "submit button scrape failed"))
  (unless (find-input 'field *script-field-label* f)
    (bail populate-se-form "groovy script field scrape failed"))
  (when infile
    (unless (find-input 'field *infile-field-label* f)
      (bail populate-se-form "input data file field scrape failed"))
    (setf (input-value (find-input 'field *infile-field-label* f)) infile))
  (setf (input-value (find-input 'field *script-field-label* f)) script-string)
  (setf (input-submit (find-input 'button *se-submit-button-label* f)) t))


(defun post-se-form-and-download-response (f basename)
  (multiple-value-bind (strm hdrs stat) (postform f :want-stream t)
    (when (and (integerp stat) (= 200 stat))
      (download strm (s+ basename (get-filename-extension hdrs)))
      (close strm))))


(defun rungroovy (id &key
                       (script-string "") (outfile-basename "groovy-out") infile
                       node-id-as-outfile-prefix avoid-nodes (tries 1))
  "Run a groovy script via app id's Script Execution form and download the document
  returned by the app. Returns the id of the node that processed the post. On failure
  returns values nil and a description."
  (with-form (f *scriptexec-path* id)
    (let* ((node-id (get-node-id f))
           (basename (if (stringp node-id)
                         (if node-id-as-outfile-prefix
                             (s+ node-id "-" outfile-basename)
                             outfile-basename)
                         (bail rungroovy "get-node-id failed"))))
      (cond
        ((> tries *find-new-node-retry-limit*)
         (values nil "find-new-node-retry-limit exceeded"))
        ((not (member node-id avoid-nodes :test #'string=))
         (multiple-value-bind (r m) (populate-se-form f script-string infile)
           (unless r (bail rungroovy m)))
         (if (post-se-form-and-download-response f basename)
             node-id
             (values nil "postform failed")))
        (t
         (rungroovy id
                    :script-string script-string :outfile-basename outfile-basename
                    :infile infile :node-id-as-outfile-prefix node-id-as-outfile-prefix
                    :avoid-nodes avoid-nodes :tries (1+ tries)))))))


(defun allgroovy (id &key (script-string "") (outfile-basename "groovy-out") infile)
  (do ((stoplen (target-nodecount (get-target id)))
       (nodes-done nil (cons (car rg-result) nodes-done))
       (rg-result '("")))
      ((or (>= (length nodes-done) stoplen)
           (not (stringp (car rg-result))))
       (if (>= (length nodes-done) stoplen) nodes-done (apply #'values rg-result)))
    (setq rg-result (multiple-value-list (rungroovy id
                                                    :script-string script-string
                                                    :outfile-basename outfile-basename
                                                    :infile infile
                                                    :node-id-as-outfile-prefix t
                                                    :avoid-nodes nodes-done)))))
