(in-package :asdf-user)

(defsystem :schlep
  :author "Darren Racine <dracine@ibc.doi.gov>"
  :description "Automates a time & attendance app via its web UI"
  :depends-on (:cl-ppcre :ironclad :patron :drakma :plump)
  :serial t
  :components
  ((:file "conf")
   (:module "base"
            :serial t
            :components
            ((:file "primitives")
             (:file "crypt")
             (:file "rc")
             (:file "threadpool")))
   (:module "web"
            :serial t
            :components
            ((:file "web-util")
             (:file "authenticate")
             (:file "runmode")
             (:file "script-exec")
             (:file "version")))
   (:module "cli"
            :serial t
            :components
            ((:file "cli-util")
             (:file "help")
             (:file "target")
             (:file "login")
             (:file "lock-unlock")
             (:file "rungroovy")
             (:file "exec")
             (:file "about")
             (:file "top")))))
