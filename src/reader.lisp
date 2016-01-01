(in-package :cl-user)

(defpackage :nbody.reader
  (:use :common-lisp
         :cl-ppcre))
  
(in-package :nbody.reader)

(defun |#p-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr (read-char stream) (read-char stream)))
        ((char= #\} curr))
      (push curr chars))
    (read-from-string 
     (destructuring-bind (name a e i o p m0 mass)
           (split "  *" (string-trim " " (coerce (nreverse chars) 'string)))
       (format nil "(setf (gethash '~a nbody:system) (nbody:create-state '(~a ~a [d~a] [d~a] [d~a] [d~a]) ~a))" name a e i o p m0 mass))
     nil nil)))

(unless (get-macro-character #\{)
  (make-dispatch-macro-character #\{))

(set-dispatch-macro-character #\{ #\p #'|#p-reader|)
