(defpackage :dci.transfer-money
  (:use :cl)
  (:export :insufficient-funds-error
           :transfer-to))
(in-package :dci.transfer-money)

(define-condition insufficient-funds-error ()
  ())

(defun transfer-to (source destination amount)
  ;; (begin-transaction)
  (when (< (dci.account:account-balance source) amount)
    ;;(end-transaction)
    (error 'insufficient-funds-error))
  (dci.account:decrease-balance source amount)
  (dci.account:increase-balance destination amount)
  (dci.account:update-log source "Transfer out")
  (dci.account:update-log destination "Transfer in")
  ;(dci.gui:display-screen :success-deposit-screen)
  ;(end-transaction)
  )
