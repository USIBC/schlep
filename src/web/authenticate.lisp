;;; schlep/src/web/authenticate.lisp
;;; D. Racine 20170425

(in-package :schlep)


(defun log-into (id)
  "Log into the webapp and return values successp, username, message, cookie-jar"
  (let* ((x (get-target id))
         (u (target-user x))
         (cj (make-instance 'cookie-jar)))
    (multiple-value-bind (body statcode headers uri stream mustclose statmsg)
        (http-request (s+ (target-url x) *login-path*) :method :post
                      :parameters (make-login-postdata u (target-pass x))
                      :cookie-jar cj :connection-timeout *login-timeout*)
      (declare (ignore headers uri stream mustclose statmsg))
      (cond
        ((and (= statcode 200) (find "JSESSIONID" (cookie-jar-cookies cj)
                                     :key #'cookie-name :test #'string=))
         (values t u "login OK" cj))
        ((= statcode 404) (values nil u "app broken - 404" nil))
        ((scan "UNAVAILABLE" body) (values nil u "app offline" nil))
        ((scan "INVALID URL" body) (values nil u "invalid url" nil))
        ((scan "password has expired" body) (values nil u "expired password" nil))
        ((scan "Invalid authentication" body) (values nil u "bad password or userid" nil))
        (t (values nil u "unknown login response" nil))))))


(defun get-session (id)
  "Returns a cookie-jar with an active session, or values nil and a message
  if authentication fails."
  (multiple-value-bind (s u m c) (log-into id)
    (declare (ignore u))
    (if s c (values nil m))))
