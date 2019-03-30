(defpackage :dci.money-transfer
  (:use :cl)
  (:export :run))
(in-package :dci.money-transfer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-condition insufficient-funds-error ()
  ())

(defun transfer-to (source destination amount)
  ;; (begin-transaction)
  (when (< (account-balance source) amount)
    ;;(end-transaction)
    (error 'insufficient-funds-error))
  (decrease-balance source amount)
  (increase-balance destination amount)
  (update-log source "Transfer out")
  (update-log destination "Transfer in")
  ;(dci.gui:display-screen :success-deposit-screen)
  ;(end-transaction)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass savings-account (account) ())

(defclass checking-account (account) ())


(defun run ()
  (let ((savings
         (make-instance 'savings-account :id 1 :balance 0))
        (checking
         (make-instance 'checking-account :id 2 :balance 0)))
    (increase-balance savings 10000)
    (transfer-to savings checking 200)
    (assert (= (account-balance savings) 9800))
    (assert (= (account-balance checking) 200))))
