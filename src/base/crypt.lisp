;;; schlep/src/base/crypt.lisp
;;; D. Racine 20170425

(in-package :schlep)

(defparameter *padlength* 16)


(defun make-key (salt pass)
  "Returns a 16-element key based on arbitrary salt and password strings"
  (ironclad:derive-key (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
                       (ironclad:ascii-string-to-byte-array pass)
                       (ironclad:ascii-string-to-byte-array salt) 4 16))


(defun get-cipher (key) (ironclad:make-cipher :aes :mode :ecb :key key))


(defun encrypt (pt salt pass)
  (let* ((cipher (get-cipher (make-key salt pass)))
         (pad (make-string *padlength* :initial-element #\.))
         (msg (ironclad:ascii-string-to-byte-array (s+ pt pad))))
    (ironclad:encrypt-in-place cipher msg)
    (ironclad:octets-to-integer msg)))


(defun decrypt (ct-int salt pass)
  (let ((cipher (get-cipher (make-key salt pass)))
        (msg (ironclad:integer-to-octets ct-int)))
    (ironclad:decrypt-in-place cipher msg)
    (subseq (coerce (mapcar #'code-char (coerce msg 'list)) 'string)
            0 (- (length msg) *padlength*))))
