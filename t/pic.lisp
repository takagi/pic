#|
  This file is a part of pic project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage pic-test
  (:use :cl
        :pic
        :prove))
(in-package :pic-test)

(plan nil)


;;;
;;; Immediates optimization
;;;

(is (pic::immediates (pic::empty-immediates-environment) '(let ((x (pic::set 1)))
                                                           (pic::mov x)))
    `(pic::set 1))


;;;
;;; Register assignment
;;;

(is (pic::register-environment-input-register 'x '((pic::call foo x)))
    :I0)

(is (pic::register-environment-input-register 'x '((pic::call foo x x)))
    nil)

(is (pic::register-environment-input-register 'x '((let ((y (pic::mov x)))
                                                     (pic::call foo x))))
    :I0)

(is (pic::register-environment-input-register 'x '((let ((y (pic::call foo x)))
                                                     (pic::mov x))))
    nil)

(is (pic::register-environment-input-register 'x '((pic::ifeq x x
                                                     (pic::call foo x)
                                                     (pic::call bar x))))
    :I0)

(is (pic::register-environment-input-register 'x '((pic::ifeq x x
                                                     (pic::call foo x)
                                                     (pic::call bar y x))))
    nil)

(is (pic::register-environment-input-register 'x '((pic::ifeq x x
                                                     (let ((y (pic::call foo x)))
                                                       (pic::mov x))
                                                     (pic::call bar x))))
    nil)

(is (pic::register-environment-input-register 'x '((pic::ifeq x x
                                                     (pic::call foo x)
                                                     (pic::mov x))))
    :I0)


(finalize)
