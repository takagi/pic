#|
  This file is a part of pic project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :pic)

;;;
;;; LED example
;;;

(defpic mdelay1 ()
  (loop #xf8                            ; 0xF8 is a magic number to delay
    (nop)))                             ;   for 1 msec

(defpicmacro with-mdelay1 (body)
  `(let ((mdelay1 ()                    ; define MDELAY1 locally to be inlined
           (loop #xf8
             (nop))))
     ,body))

;; (defpicmacro mdelay (n)
;;   (unless (<= 0 n 65535)
;;     (error "The value ~S is invalid." n))
;;   (multiple-value-bind (q r) (truncate n 256)
;;     (if (= q 0)
;;         `(loop ,r (mdelay1))
;;         `(loop ,q (loop ,r (mdelay1))))))

(defpicmacro mdelay (n)
  (unless (<= 0 n 65535)
    (error "The value ~S is invalid." n))
  (multiple-value-bind (q r) (truncate n 256)
    (if (= q 0)
        `(with-mdelay1
           (loop ,r (mdelay1)))
        `(with-mdelay1
           (loop ,q (loop ,r (mdelay1)))))))

(defpic init ()
  (progn
    (setreg :gpio #x0)                  ; clera GP0-5
    (setreg :cmcon0 #x07)               ; disable comparator
    (setbank1)                          ; switch to bank 1
    (setreg :trisio #x08)               ; only GP3 is input mode
    (setreg :ansel #x00)                ; disable analog IO
    (setreg :ioc #x00)                  ; disable interruption
    (setbank0)                          ; switch to bank 0
    (setreg :intcon #x00)))             ; disable interruption

(defpic main ()
  (progn
    (setreg :gpio #x20)                 ; set GP5 to high
    (mdelay 50)                         ; delay for 50 msec
    (setreg :gpio #x00)                 ; set GP5 to low
    (mdelay 950)                        ; delay for 950 msec
    (main)))                            ; repeat
