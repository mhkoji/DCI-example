(defpackage :dci
  (:use :cl)
  (:export :app))
(in-package :dci)

(defclass savings-account (dci.account:account) ())

(defclass checking-account (dci.account:account) ())


(defun app ()
  (let ((savings (make-instance 'savings-account
                                :id 1 :balance 0))
        (checking (make-instance 'checking-account
                                 :id 2 :balance 0)))
    (dci.account:increase-balance savings 10000)
    (dci.transfer-money:transfer-to savings checking 200)
    (assert (= (dci.account:account-balance savings) 9800))
    (assert (= (dci.account:account-balance checking) 200))))
(app)
