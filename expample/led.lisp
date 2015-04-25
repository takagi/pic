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

(defpicmacro mdelay (n)
  (check-type n (unsigned-byte 16))
  (multiple-value-bind (q r) (truncate n 256)
    (cond
      ((and (> q 0) (> r 0)) `(progn
                               (loop ,q (loop 0 (mdelay1)))
                               (loop ,r (mdelay1))))
      ((> q 0) `(loop ,q (loop 0 (mdelay1))))
      ((> r 0) `(loop ,r (mdelay1)))
      (t '(nop)))))

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
