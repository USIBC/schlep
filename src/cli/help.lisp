;;; schlep/src/cli/help.lisp
;;; D. Racine 20170426

(in-package :schlep)


(defun usage (&key no-more-info)
  (format t "~%~a~%"
          "  usage: schlep command [arguments] [targets]

  Applies 'command [arguments]' to target app instances.")
  (if no-more-info
      (format t "~%")
      (format t "~a~%~%" "  See 'schlep help' for more info.")))


(defun target-help ()
  (format t "~%~a~%~%"
          (s+ "  schlep target list
  schlep target remove id
  schlep target add id baseURL nodecount user password

  List, remove, or add webapp instances in|from|to " *rcfile* "

  When adding a target, baseURL should have the following form,
  without a trailing '/' character:  https://name.domain/appid")))


(defun login-help ()
  (format t "~%~a~%~%"
          "  schlep login targets

  Log into the specified target webapp instances and report any failures."))


(defun runmode-help ()
  (format t "~%~a~%~%"
          "  schlep showmode targets
  schlep lock targets
  schlep unlock targets

  Display or set the Run Mode of the specified target webapp instances."))


(defun rungroovy-help ()
  (format t "~%~a~%~%"
          "  schlep rungroovy scriptfile|- [inputfile] targets

  Runs scriptfile with an optional input file via each target webapp
  instance's Script Execution screen and reports any failures. 

  The outputs of the script executions are written to files in the PWD with
  names of the form {target_id}-{scriptfile_basename}-out.{ext}

  The content of each output file will be formatted according to the webapp's
  interpretation of the output directives in the groovy script, e.g. HTML, CSV,
  or MSO document.

  Scripts automated via schlep should not include interactive dialogs. When
  necessary, remove or comment out message invocations in the copy of the
  script that will be fed to rungroovy or allgroovy.

  To run a groovy script fed to schlep's standard input, use a '-' character
  in place of the 'scriptfile' parameter.

  Script execution is considered successful if schlep receives an HTTP 200 in
  response to POSTing the Script Execution form."))


(defun allgroovy-help ()
  (format t "~%~a~%~%"
          "  schlep allgroovy scriptfile|- [inputfile] targets

  Runs scriptfile with an optional input file on all running nodes
  of each target webapp instance and reports any failures. Does not
  shut down any webapp nodes.

  The outputs of the script executions are written to files in the PWD with
  names of the form {target_node_id}-{scriptfile_basename}-out.{ext}

  The content of each output file will be formatted according to the webapp's
  interpretation of the output directives in the groovy script, e.g. HTML, CSV,
  or MSO document.

  Scripts automated via schlep should not include interactive dialogs. When
  necessary, remove or comment out message invocations in the copy of the
  script that will be fed to rungroovy or allgroovy.

  To run a groovy script fed to schlep's standard input, use a '-' character
  in place of the 'scriptfile' parameter.

  Script execution is considered successful if schlep receives an HTTP 200 in
  response to POSTing the Script Execution form."))


(defun exec-help ()
  (format t "~%~a~%~%"
          "  schlep exec lispfile|- [args] [targets]

  Loads the schlep extension defined in lispfile in the 'schlep' package.
  This is a hook for executing arbitrary Common Lisp code. All command line
  arguments and targets specified after lispfile are available to the code
  in lispfile via the *exec-args* variable.

  To run an extension fed to schlep's standard input, use a '-' character
  in place of the 'lispfile' argument."))


(defun help-cmd (&optional arg)
  (flet ((arg-is (s) (string= arg s)))
    (if (stringp arg)
        (cond
          ((arg-is "help") (help-cmd))
          ((arg-is "target") (target-help))
          ((arg-is "login") (login-help))
          ((or (arg-is "showmode")
               (arg-is "lock")
               (arg-is "unlock")) (runmode-help))
          ((arg-is "rungroovy") (rungroovy-help))
          ((arg-is "allgroovy") (allgroovy-help))
          ((arg-is "exec") (exec-help))
          (t (stderr (s+ "unknown command " (prin1-to-string arg)) 2)))
        (progn (usage :no-more-info t)
               (format t "~a~%~%"
                       "  command    arguments
  ---------  --------------------------------------
  allgroovy  scriptfile|- [inputfile]
  exec       lispfile|- [args]
  help       [command]
  lock
  login
  rungroovy  scriptfile|- [inputfile]
  showmode
  target     list
             remove id
             add id baseURL nodecount user password
  unlock")))))


(defhandler "help" (lambda (l) (help-cmd (car l))))
