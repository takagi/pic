#|
  This file is a part of pic project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage pic-test-asd
  (:use :cl :asdf))
(in-package :pic-test-asd)

(defsystem pic-test
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:pic
               :prove)
  :components ((:module "t"
                :components
                ((:file "pic"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
