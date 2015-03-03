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

(is (pic::register-environment-input-register 'x '((loop 5
                                                     (pic::call foo x))))
    nil
    "Variables used for function call parameters in LOOP instructions are not assigned to input registers.")

(let ((form '(let ((tmp0 (set 1)))
               (loop tmp0
                 (let ((tmp1 (set 1)))
                   (loop tmp1
                     (let ((tmp2 (set 1)))
                       (loop tmp2
                         (let ((tmp3 (set 1)))
                           (loop tmp3
                             (let ((tmp4 (set 1)))
                               (loop tmp4
                                 (let ((tmp5 (set 1)))
                                   (loop tmp5
                                     (let ((tmp6 (set 1)))
                                       (loop tmp6
                                         (let ((tmp7 (set 1)))
                                           (loop tmp7
                                             (let ((tmp8 (set 1)))
                                               tmp8)))))))))))))))))))
  (is-error (pic::assign (pic::empty-register-environment) nil form)
            'simple-error
            "LOOP instructions keep couter registers in their scope."))


(finalize)
