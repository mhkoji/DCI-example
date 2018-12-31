(defpackage :dci.account
  (:use :cl)
  (:export :account
           :account-balance
           :increase-balance
           :decrease-balance
           :update-log))
(in-package :dci.account)

(defclass account ()
  ((id
    :initarg :id
    :reader account-id)
   (balance
    :initarg :balance
    :accessor account-balance)))

(defun increase-balance (account amount)
  (incf (account-balance account) amount))

(defun decrease-balance (account amount)
  (decf (account-balance account) amount))

(defun update-log (account msg)
  (format *standard-output* "Account [~A]: ~A~%"
          (account-id account)
          msg))
