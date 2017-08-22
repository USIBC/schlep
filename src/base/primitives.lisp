;;; schlep/src/base/primitives.lisp
;;; D. Racine 20170426

(in-package :schlep)


(defmacro s+ (&rest args) `(concatenate 'string ,@args))


(defmacro bail (blocktag message)
  `(return-from ,blocktag (values nil ,message)))


(defun stderr (message &optional exit-status-int)
  "Print message to stderr. Quit if an exit status integer is specified."
  (format *error-output* "~a ~a~%" "schlep:" message)
  (when (integerp exit-status-int) (sb-ext:exit :code exit-status-int)))


(defun basename (namestr)
  (regex-replace "\\.[^.]*$" (car (last (split "/" namestr))) ""))


(defmacro findlist (keystring list-of-lists-that-start-with-strings)
  `(find ,keystring
         ,list-of-lists-that-start-with-strings
         :key #'car :test #'string-equal))


(defun stream-copy (in out element-type)
  (loop with buf = (make-array 4096 :element-type element-type)
     for pos = (read-sequence buf in)
     while (plusp pos) do (write-sequence buf out :end pos)))


(defun download (instream filepath)
  (with-open-file (outstream filepath :direction :output :if-exists :rename
                             :element-type '(unsigned-byte 8))
    (stream-copy instream outstream '(unsigned-byte 8))))


(defun slurp-string (x)
  "Returns a string read from a file or input stream"
  (with-output-to-string (out)
    (cond ((streamp x) (stream-copy x out 'character))
          ((probe-file x) (with-open-file (in x) (stream-copy in out 'character)))
          (t nil))))
