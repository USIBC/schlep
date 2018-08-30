;;; schlep/src/base/crypt.lisp
;;; D. Racine 20170425

(in-package :schlep)


(defun make-key (salt pass)
  "Returns a 16-element key based on arbitrary salt and password strings"
  (ironclad:derive-key (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
                       (ironclad:ascii-string-to-byte-array pass)
                       (ironclad:ascii-string-to-byte-array salt) 4 16))


(defun get-cipher (key)
  (ironclad:make-cipher
   :aes :mode :ofb :key key
   :initialization-vector (make-array '(16)
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 0)))


(defun encrypt (pt salt pass)
  (let* ((cipher (get-cipher (make-key salt pass)))
         (msg (ironclad:ascii-string-to-byte-array pt)))
    (ironclad:encrypt-in-place cipher msg)
    (ironclad:octets-to-integer msg)))


(defun decrypt (ct-int salt pass)
  (let ((cipher (get-cipher (make-key salt pass)))
        (msg (ironclad:integer-to-octets ct-int)))
    (ironclad:decrypt-in-place cipher msg)
    (coerce (mapcar #'code-char (coerce msg 'list)) 'string)))
