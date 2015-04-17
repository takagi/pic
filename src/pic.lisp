#|
  This file is a part of pic project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage pic
  (:use :cl)
  (:export ;; Programming Interfaces
           :defpic
           :defpicmacro
           :pic-compile
           :pic-disassemble
           :pic-macroexpand
           :pic-macroexpand1
           :pic-clear
           ;; Syntax
           :setreg
           ;; Special Functions
           :init
           :main
           :intr
           ;; Macros
           :progn
           :loop
           :setbank0
           :setbank1)
  (:import-from :alexandria
                :symbolicate
                :make-keyword
                :hash-table-keys))
(in-package :pic)


;;;
;;; Utilities
;;;

(defun singlep (object)
  (and (consp object)
       (null (cdr object))))

(defun flatten-list (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun compile-token (token)
  (cond
    ((pic-symbol-p token)
     (let ((token1 (substitute #\_ #\- (symbol-name token))))
       (if (keywordp token)
           token1
           (format nil "_~A" token1))))
    ((pic-int-p token) (format nil "~3,'0Xh" token))
    (t (error "The value ~S is an invalid token." token))))

(defun make-unique-symbol (object)
  (gensym (princ-to-string object)))


;;;
;;; Data structure
;;;

(defun pic-symbol-p (object)
  (and object
       (symbolp object)))

(defun pic-reg-p (object)
  (and (pic-symbol-p object)
       (keywordp object)))

(defun pic-int-p (object)
  (and (integerp object)
       (<= 0 object 255)))


;;;
;;; Syntax
;;;

(defun literal-p (object)
  (pic-int-p object))

(defun reference-p (object)
  (pic-symbol-p object))

(defun sub-p (object)
  (cl-pattern:match object
    (('- . _) t)
    (_ nil)))

(defun sub-expr1 (form)
  (cl-pattern:match form
    (('- expr1 _) expr1)
    (('- . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun sub-expr2 (form)
  (cl-pattern:match form
    (('- _ expr2) expr2)
    (('- . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun ifeq-p (object)
  (cl-pattern:match object
    (('if . _) t)
    (_ nil)))

(defun ifeq-lhs (form)
  (cl-pattern:match form
    (('if ('= lhs _) _ _) lhs)
    (('if . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun ifeq-rhs (form)
  (cl-pattern:match form
    (('if ('= _ rhs) _ _) rhs)
    (('if . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun ifeq-then (form)
  (cl-pattern:match form
    (('if ('= _ _) then _) then)
    (('if . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun ifeq-else (form)
  (cl-pattern:match form
    (('if ('= _ _) _ else) else)
    (('if . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun let-p (object)
  (cl-pattern:match object
    (('let ((_ _)) _) t)
    (_ nil)))

(defun let-var (form)
  (cl-pattern:match form
    (('let ((var _)) _)
     (if (pic-symbol-p var)
         var
         (error "The form ~S is malformed." form)))
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun let-expr (form)
  (cl-pattern:match form
    (('let ((_ expr)) _) expr)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun let-body (form)
  (cl-pattern:match form
    (('let ((_ _)) body) body)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun letrec-p (object)
  (cl-pattern:match object
    (('let ((_ _ _)) _) t)
    (_ nil)))

(defun letrec-name (form)
  (cl-pattern:match form
    (('let ((name _ _)) _)
     (if (pic-symbol-p name)
         name
         (error "The form ~S is malformed." form)))
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun letrec-args (form)
  (cl-pattern:match form
    (('let ((_ args _)) _)
     (if (every #'pic-symbol-p args)
         args
         (error "The form ~S is malformed." form)))
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun letrec-expr (form)
  (cl-pattern:match form
    (('let ((_ _ expr)) _) expr)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun letrec-body (form)
  (cl-pattern:match form
    (('let ((_ _ _)) body) body)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun with-args-p (object)
  (cl-pattern:match object
    (('with-args . _) t)
    (_ nil)))

(defun with-args-args (form)
  (cl-pattern:match form
    (('with-args args _) args)
    (('with-args . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun with-args-body (form)
  (cl-pattern:match form
    (('with-args _ body) body)
    (('with-args . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun loop-p (object)
  (cl-pattern:match object
    (('loop . _) t)
    (_ nil)))

(defun loop-times (form)
  (cl-pattern:match form
    (('loop times _) times)
    (('loop . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun loop-body (form)
  (cl-pattern:match form
    (('loop _ body) body)
    (('loop . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun setreg-p (object)
  (cl-pattern:match object
    (('setreg . _) t)
    (_ nil)))

(defun setreg-reg (form)
  (cl-pattern:match form
    (('setreg reg _)
     (if (pic-reg-p reg)
         reg
         (error "The form ~S is malformed." form)))
    (('setreg . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun setreg-expr (form)
  (cl-pattern:match form
    (('setreg _ expr) expr)
    (('setreg . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun apply-p (object)
  (cl-pattern:match object
    ((name . _) (pic-symbol-p name))
    (_ nil)))

(defun apply-operator (form)
  (unless (apply-p form)
    (error "The value ~S is an invalid form." form))
  (car form))

(defun apply-operands (form)
  (unless (apply-p form)
    (error "The value ~S is an invalid form." form))
  (cdr form))

(defun macro-p (form)
  (cl-pattern:match form
    ((name . _) (and (symbolp name)
                     (get name 'pic-macro)
                     t))
    (_ nil)))

(defun macro-name (form)
  (unless (macro-p form)
    (error "The value ~S is an invalid.form." form))
  (car form))

(defun macro-operands (form)
  (unless (macro-p form)
    (error "The value ~S is an invalid.form." form))
  (cdr form))

(defun macro-expander (form)
  (unless (macro-p form)
    (error "The value ~S is an invalid.form." form))
  (get (macro-name form) 'pic-macro))


;;;
;;; Virtual Machine Instructions
;;;

(defun let-inst-p (object)
  (cl-pattern:match object
    (('let ((_ _)) _) t)
    (_ nil)))

(defun let-inst-var (form)
  (cl-pattern:match form
    (('let ((var _)) _) var)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun let-inst-expr (form)
  (cl-pattern:match form
    (('let ((_ expr)) _) expr)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun let-inst-body (form)
  (cl-pattern:match form
    (('let ((_ _)) body) body)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun letrec-inst-p (object)
  (cl-pattern:match object
    (('let ((_ _ _)) _) t)
    (_ nil)))

(defun letrec-inst-name (form)
  (cl-pattern:match form
    (('let ((name _ _)) _) name)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun letrec-inst-args (form)
  (cl-pattern:match form
    (('let ((_ args _)) _) args)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun letrec-inst-expr (form)
  (cl-pattern:match form
    (('let ((_ _ expr)) _) expr)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))  

(defun letrec-inst-body (form)
  (cl-pattern:match form
    (('let ((_ _ _)) body) body)
    (('let . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))  

(defun set-inst-p (object)
  (cl-pattern:match object
    (('set . _) t)
    (_ nil)))

(defun set-inst-literal (form)
  (cl-pattern:match form
    (('set literal) literal)
    (('set . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun mov-inst-p (object)
  (cl-pattern:match object
    (('mov . _) t)
    (_ nil)))

(defun mov-inst-reg (form)
  (cl-pattern:match form
    (('mov reg) reg)
    (('mov . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun sub-inst-p (object)
  (cl-pattern:match object
    (('sub . _) t)
    (_ nil)))

(defun sub-inst-expr1 (form)
  (cl-pattern:match form
    (('sub expr1 _) expr1)
    (('sub . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun sub-inst-expr2 (form)
  (cl-pattern:match form
    (('sub _ expr2) expr2)
    (('sub . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun ifeq-inst-p (object)
  (cl-pattern:match object
    (('ifeq . _) t)
    (_ nil)))

(defun ifeq-inst-lhs (form)
  (cl-pattern:match form
    (('ifeq lhs _ _ _) lhs)
    (('ifeq . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun ifeq-inst-rhs (form)
  (cl-pattern:match form
    (('ifeq _ rhs _ _) rhs)
    (('ifeq . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun ifeq-inst-then (form)
  (cl-pattern:match form
    (('ifeq _ _ then _) then)
    (('ifeq . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun ifeq-inst-else (form)
  (cl-pattern:match form
    (('ifeq _ _ _ else) else)
    (('ifeq . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun loop-inst-p (object)
  (loop-p object))

(defun loop-inst-var (form)
  (loop-times form))

(defun loop-inst-body (form)
  (loop-body form))

(defun call-inst-p (object)
  (cl-pattern:match object
    (('call . _) t)
    (_ nil)))

(defun call-inst-name (form)
  (cl-pattern:match form
    (('call name . _) name)
    (('call . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun call-inst-operands (form)
  (cl-pattern:match form
    (('call _ . operands) operands)
    (('call . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun with-save-inst-p (object)
  (cl-pattern:match object
    (('with-save . _) t)
    (_ nil)))

(defun with-save-inst-regs (form)
  (cl-pattern:match form
    (('with-save regs _) regs)
    (('with-save . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun with-save-inst-expr (form)
  (cl-pattern:match form
    (('with-save _ expr) expr)
    (('with-save . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun restore-inst-p (object)
  (cl-pattern:match object
    (('restore . _) t)
    (_ nil)))

(defun restore-inst-regs (form)
  (cl-pattern:match form
    (('restore . regs) regs)
    (_ (error "The value ~S is an invalid form." form))))

(defun setreg-inst-p (object)
  (cl-pattern:match object
    (('setreg . _) t)
    (_ nil)))

(defun setreg-inst-reg (form)
  (cl-pattern:match form
    (('setreg reg _) reg)
    (('setreg . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))

(defun setreg-inst-expr (form)
  (cl-pattern:match form
    (('setreg _ expr) expr)
    (('setreg . _) (error "The form ~S is malformed." form))
    (_ (error "The value ~S is an invalid form." form))))


;;;
;;; Macro expansion
;;;

(defun expand (form)
  (cond
    ((macro-p form) (expand-macro form))
;    ((literal-p form) (expand-literal form))
;    ((reference-p form) (expand-reference form))
    ((sub-p form) (expand-sub form))
    ((ifeq-p form) (expand-ifeq form))
    ((let-p form) (expand-let form))
    ((letrec-p form) (expand-letrec form))
    ((with-args-p form) (expand-with-args form))
    ((loop-p form) (expand-loop form))
    ((setreg-p form) (expand-setreg form))
    ((apply-p form) (expand-apply form))
    (t (expand-default form))))

(defun expand-macro (form)
  (let ((expander (macro-expander form))
        (operands (macro-operands form)))
    (let ((operands1 (mapcar #'expand operands)))
      (expand (apply expander operands1)))))

(defun expand-default (form)
  form)

(defun expand-literal (form)
  form)

(defun expand-reference (form)
  form)

(defun expand-sub (form)
  (let ((expr1 (sub-expr1 form))
        (expr2 (sub-expr2 form)))
    (let ((expr1% (expand expr1))
          (expr2% (expand expr2)))
      `(- ,expr1% ,expr2%))))

(defun expand-ifeq (form)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (let ((lhs1 (expand lhs))
          (rhs1 (expand rhs))
          (then1 (expand then))
          (else1 (expand else)))
      `(if (= ,lhs1 ,rhs1) ,then1 ,else1))))

(defun expand-let (form)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (let ((expr1 (expand expr))
          (body1 (expand body)))
      `(let ((,var ,expr1))
         ,body1))))

(defun expand-letrec (form)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (let ((expr1 (expand expr))
          (body1 (expand body)))
      `(let ((,name ,args ,expr1))
         ,body1))))

(defun expand-with-args (form)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (let ((body1 (expand body)))
      `(with-args ,args
         ,body1))))

(defun expand-loop (form)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (let ((times1 (expand times))
          (body1 (expand body)))
      `(loop ,times1
         ,body1))))

(defun expand-setreg (form)
  (let ((reg (setreg-reg form))
        (expr (setreg-expr form)))
    (let ((expr1 (expand expr)))
      `(setreg ,reg ,expr1))))

(defun expand-apply (form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operands1 (mapcar #'expand operands)))
      `(,operator ,@operands1))))


;;;
;;; K-normalization
;;;

(defun empty-k-normal-environment ()
  nil)

(defun k-normal-environment-add (var env)
  (cons var env))

(defun k-normal-environment-add-list (vars env)
  (if vars
      (destructuring-bind (var . rest) vars
        (k-normal-environment-add-list rest
         (k-normal-environment-add var env)))
      env))

(defun k-normal-environment-exists-p (var env)
  (and (member var env)
       t))

(defun k-normal (env form)
  (cond
    ((literal-p form) (k-normal-literal form))
    ((reference-p form) (k-normal-reference env form))
    ((sub-p form) (k-normal-sub env form))
    ((ifeq-p form) (k-normal-ifeq env form))
    ((let-p form) (k-normal-let env form))
    ((letrec-p form) (k-normal-letrec env form))
    ((with-args-p form) (k-normal-with-args env form))
    ((loop-p form) (k-normal-loop env form))
    ((setreg-p form) (k-normal-setreg env form))
    ((apply-p form) (k-normal-apply env form))
    (t (error "The value ~S is an invalid form." form))))

(defun k-normal-literal (form)
  form)

(defun k-normal-reference (env form)
  (if (k-normal-environment-exists-p form env)
      form
      (error "The variable ~S not found." form)))

(defun k-normal-sub (env form)
  (let ((expr1 (sub-expr1 form))
        (expr2 (sub-expr2 form)))
    (let ((expr1% (k-normal env expr1))
          (expr2% (k-normal env expr2)))
      `(let ((tmpa ,expr1%))
         (let ((tmpb ,expr2%))
           (- tmpa tmpb))))))

(defun k-normal-ifeq (env form)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (let ((lhs1 (k-normal env lhs))
          (rhs1 (k-normal env rhs))
          (then1 (k-normal env then))
          (else1 (k-normal env else)))
      `(let ((tmpa ,lhs1))
         (let ((tmpb ,rhs1))
           (if (= tmpa tmpb)
               ,then1
               ,else1))))))

(defun k-normal-let (env form)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (let ((env1 (k-normal-environment-add var env)))
      (let ((expr1 (k-normal env expr))
            (body1 (k-normal env1 body)))
        `(let ((,var ,expr1))
           ,body1)))))

(defun k-normal-letrec (env form)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (let ((env1 (k-normal-environment-add-list args
                 (empty-k-normal-environment))))
      (let ((expr1 (k-normal env1 expr))
            (body1 (k-normal env body)))
        `(let ((,name ,args ,expr1))
           ,body1)))))

(defun k-normal-with-args (env form)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (let* ((env1 (k-normal-environment-add-list args env))
           (body1 (k-normal env1 body)))
      `(with-args ,args ,body1))))

(defun k-normal-loop (env form)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (let ((times1 (k-normal env times))
          (body1 (k-normal env body)))
      `(let ((tmp ,times1))
         (loop tmp
           ,body1)))))

(defun k-normal-setreg (env form)
    (let ((reg (setreg-reg form))
        (expr (setreg-expr form)))
    (let ((expr1 (k-normal env expr)))
      `(let ((tmp ,expr1))
         (setreg ,reg tmp)))))

(defun k-normal-apply (env form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (k-normal-apply% env operator operands 0 nil)))

(defun k-normal-apply% (env operator operands i tmps)
  (if operands
      (let ((operand (car operands))
            (rest (cdr operands)))
        (let ((operand1 (k-normal env operand)))
          (let* ((tmp (tmp-var i))
                 (tmps (cons tmp tmps)))
            `(let ((,tmp ,operand1))
               ,(k-normal-apply% env operator rest (1+ i) tmps)))))
      `(,operator ,@(nreverse tmps))))

(defun tmp-var (i)
  (symbolicate "TMP" (code-char (+ 65 i))))


;;;
;;; Alpha-conversion for variables
;;;

(defun empty-alpha1-environment ()
  nil)

(defun alpha1-environment-add (var env)
  (acons var (make-unique-symbol var) env))

(defun alpha1-environment-add-list (vars env)
  (if vars
      (destructuring-bind (var . rest) vars
        (alpha1-environment-add-list rest
         (alpha1-environment-add var env)))
      env))

(defun alpha1-environment-lookup (var env)
  (or (cdr (assoc var env))
      var))

(defun alpha1 (env form)
  (cond
    ((literal-p form) (alpha1-literal form))
    ((reference-p form) (alpha1-reference env form))
    ((sub-p form) (alpha1-sub env form))
    ((ifeq-p form) (alpha1-ifeq env form))
    ((let-p form) (alpha1-let env form))
    ((letrec-p form) (alpha1-letrec env form))
    ((with-args-p form) (alpha1-with-args env form))
    ((loop-p form) (alpha1-loop env form))
    ((setreg-p form) (alpha1-setreg env form))
    ((apply-p form) (alpha1-apply env form))
    (t (error "The value ~S is an invalid form." form))))

(defun alpha1-literal (form)
  form)

(defun alpha1-reference (env form)
  (alpha1-environment-lookup form env))

(defun alpha1-sub (env form)
  (let ((expr1 (sub-expr1 form))
        (expr2 (sub-expr2 form)))
    (let ((expr1% (alpha1 env expr1))
          (expr2% (alpha1 env expr2)))
      `(- ,expr1% ,expr2%))))

(defun alpha1-ifeq (env form)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (let ((lhs1 (alpha1 env lhs))
          (rhs1 (alpha1 env rhs))
          (then1 (alpha1 env then))
          (else1 (alpha1 env else)))
      `(if (= ,lhs1 ,rhs1) ,then1 ,else1))))

(defun alpha1-let (env form)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (let ((expr1 (alpha1 env expr)))
      (let* ((env1 (alpha1-environment-add var env))
             (var1 (alpha1-environment-lookup var env1))
             (body1 (alpha1 env1 body)))
        `(let ((,var1 ,expr1))
           ,body1)))))
  
(defun alpha1-letrec (env form)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (let* ((env0 (alpha1-environment-add-list args
                  (empty-alpha1-environment)))
           (args1 (mapcar #'(lambda (arg)
                              (alpha1-environment-lookup arg env0))
                          args))
           (expr1 (alpha1 env0 expr)))
      (let ((body1 (alpha1 env body)))
        `(let ((,name ,args1 ,expr1))
           ,body1)))))

(defun alpha1-with-args (env form)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (let* ((env1 (alpha1-environment-add-list args env))
           (args1 (mapcar #'(lambda (arg)
                              (alpha1-environment-lookup arg env1))
                          args)))
      (let ((body1 (alpha1 env1 body)))
        `(with-args ,args1 ,body1)))))

(defun alpha1-loop (env form)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (let ((times1 (alpha1 env times))
          (body1 (alpha1 env body)))
      `(loop ,times1
         ,body1))))

(defun alpha1-setreg (env form)
  (let ((reg (setreg-reg form))
        (expr (setreg-expr form)))
    (let ((expr1 (alpha1 env expr)))
      `(setreg ,reg ,expr1))))

(defun alpha1-apply (env form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operands1 (mapcar #'(lambda (operand)
                                 (alpha1 env operand))
                             operands)))
      `(,operator ,@operands1))))


;;;
;;; Alpha-conversion for functions
;;;

(defun empty-alpha2-environment ()
  nil)

(defun alpha2-environment-add (name env)
  (acons name (make-unique-symbol name) env))

(defun alpha2-environment-lookup (name env)
  (or (cdr (assoc name env))
      name))

(defun alpha2 (env form)
  (cond
    ((literal-p form) (alpha2-literal form))
    ((reference-p form) (alpha2-reference form))
    ((sub-p form) (alpha2-sub env form))
    ((ifeq-p form) (alpha2-ifeq env form))
    ((let-p form) (alpha2-let env form))
    ((letrec-p form) (alpha2-letrec env form))
    ((with-args-p form) (alpha2-with-args env form))
    ((loop-p form) (alpha2-loop env form))
    ((setreg-p form) (alpha2-setreg env form))
    ((apply-p form) (alpha2-apply env form))
    (t (error "The value ~S is an invalid form." form))))

(defun alpha2-literal (form)
  form)

(defun alpha2-reference (form)
  form)

(defun alpha2-sub (env form)
  (let ((expr1 (sub-expr1 form))
        (expr2 (sub-expr2 form)))
    (let ((expr1% (alpha2 env expr1))
          (expr2% (alpha2 env expr2)))
      `(- ,expr1% ,expr2%))))

(defun alpha2-ifeq (env form)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (let ((lhs1 (alpha2 env lhs))
          (rhs1 (alpha2 env rhs))
          (then1 (alpha2 env then))
          (else1 (alpha2 env else)))
      `(if (= ,lhs1 ,rhs1) ,then1 ,else1))))

(defun alpha2-let (env form)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (let ((expr1 (alpha2 env expr))
          (body1 (alpha2 env body)))
      `(let ((,var ,expr1))
         ,body1))))

(defun alpha2-letrec (env form)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (let ((env1 (alpha2-environment-add name env)))
      (let ((name1 (alpha2-environment-lookup name env1))
            (expr1 (alpha2 env1 expr))
            (body1 (alpha2 env1 body)))
        `(let ((,name1 ,args ,expr1))
           ,body1)))))

(defun alpha2-with-args (env form)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (let ((body1 (alpha2 env body)))
      `(with-args ,args ,body1))))

(defun alpha2-loop (env form)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (let ((times1 (alpha2 env times))
          (body1 (alpha2 env body)))
      `(loop ,times1
         ,body1))))

(defun alpha2-setreg (env form)
  (let ((reg (setreg-reg form))
        (expr (setreg-expr form)))
    (let ((expr1 (alpha2 env expr)))
      `(setreg ,reg ,expr1))))

(defun alpha2-apply (env form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operator1 (alpha2-environment-lookup operator env)))
      `(,operator1 ,@operands))))


;;;
;;; Beta-reduction
;;;

(defun empty-beta-environment ()
  nil)

(defun beta-environment-add (var var1 env)
  (acons var var1 env))

(defun beta-environment-lookup (var env)
  (or (cdr (assoc var env))
      var))

(defun beta (env form)
  (cond
    ((literal-p form) (beta-literal form))
    ((reference-p form) (beta-reference env form))
    ((sub-p form) (beta-sub env form))
    ((ifeq-p form) (beta-ifeq env form))
    ((let-p form) (beta-let env form))
    ((letrec-p form) (beta-letrec env form))
    ((with-args-p form) (beta-with-args env form))
    ((loop-p form) (beta-loop env form))
    ((setreg-p form) (beta-setreg env form))
    ((apply-p form) (beta-apply env form))
    (t (error "The value ~S is an invalid form." form))))

(defun beta-literal (form)
  form)

(defun beta-reference (env form)
  (beta-environment-lookup form env))

(defun beta-sub (env form)
  (let ((expr1 (sub-expr1 form))
        (expr2 (sub-expr2 form)))
    (let ((expr1% (beta env expr1))
          (expr2% (beta env expr2)))
      `(- ,expr1% ,expr2%))))

(defun beta-ifeq (env form)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (let ((lhs1 (beta env lhs))
          (rhs1 (beta env rhs))
          (then1 (beta env then))
          (else1 (beta env else)))
      `(if (= ,lhs1 ,rhs1) ,then1 ,else1))))

(defun beta-let (env form)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (let ((expr1 (beta env expr)))
      (if (reference-p expr1)
          (let* ((env1 (beta-environment-add var expr1 env))
                 (body1 (beta env1 body)))
            body1)
          (let ((body1 (beta env body)))
            `(let ((,var ,expr1))
               ,body1))))))

(defun beta-letrec (env form)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (let ((expr1 (beta (empty-beta-environment) expr))
          (body1 (beta env body)))
      `(let ((,name ,args ,expr1))
         ,body1))))

(defun beta-with-args (env form)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (let ((body1 (beta env body)))
      `(with-args ,args ,body1))))

(defun beta-loop (env form)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (let ((times1 (beta env times))
          (body1 (beta env body)))
      `(loop ,times1
         ,body1))))

(defun beta-setreg (env form)
  (let ((reg (setreg-reg form))
        (expr (setreg-expr form)))
    (let ((expr1 (beta env expr)))
      `(setreg ,reg ,expr1))))

(defun beta-apply (env form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((operands1 (mapcar #'(lambda (operand)
                                 (beta env operand))
                             operands)))
      `(,operator ,@operands1))))


;;;
;;; Let flattening
;;;

(defun flatten (form)
  (cond
    ((literal-p form) (flatten-literal form))
    ((reference-p form) (flatten-reference form))
    ((sub-p form) (flatten-sub form))
    ((ifeq-p form) (flatten-ifeq form))
    ((let-p form) (flatten-let form))
    ((letrec-p form) (flatten-letrec form))
    ((with-args-p form) (flatten-with-args form))
    ((loop-p form) (flatten-loop form))
    ((setreg-p form) (flatten-setreg form))
    ((apply-p form) (flatten-apply form))
    (t (error "The value ~S is an invalid form." form))))

(defun flatten-literal (form)
  form)

(defun flatten-reference (form)
  form)

(defun flatten-sub (form)
  form)

(defun flatten-ifeq (form)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (let ((then1 (flatten then))
          (else1 (flatten else)))
      `(if (= ,lhs ,rhs) ,then1 ,else1))))

(defun flatten-let (form)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (if (let-p expr)
        (let ((var1 (let-var expr))
              (expr1 (let-expr expr))
              (body1 (let-body expr)))
          (flatten `(let ((,var1 ,expr1))
                      (let ((,var ,body1))
                        ,body))))
        (let ((expr1 (flatten expr))
              (body1 (flatten body)))
          `(let ((,var ,expr1))
             ,body1)))))

(defun flatten-letrec (form)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (let ((expr1 (flatten expr))
          (body1 (flatten body)))
      `(let ((,name ,args ,expr1))
         ,body1))))

(defun flatten-with-args (form)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (let ((body1 (flatten body)))
      `(with-args ,args ,body1))))

(defun flatten-loop (form)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (let ((body1 (flatten body)))
      `(loop ,times
         ,body1))))

(defun flatten-setreg (form)
  form)

(defun flatten-apply (form)
  form)


;;;
;;; Inlining
;;;

(defun empty-inlined-environment ()
  nil)

(defun inlined-environment-add (name args expr env)
  (acons name (list args expr) env))

(defun inlined-environment-add-fundefs (fundefs env)
  (if fundefs
      (destructuring-bind ((name args body) . rest) fundefs
        (inlined-environment-add-fundefs rest
         (inlined-environment-add name args body env)))
      env))

(defun inlined-environment-lookup (name env)
  (cdr (assoc name env)))

(defun inlined (fundefs form)
  (let* ((env (inlined-environment-add-fundefs fundefs
               (empty-inlined-environment))))
    (inlined% env form)))

(defun inlined% (env form)
  (cond
    ((literal-p form) (inlined-literal form))
    ((reference-p form) (inlined-reference form))
    ((sub-p form) (inlined-sub env form))
    ((ifeq-p form) (inlined-ifeq env form))
    ((let-p form) (inlined-let env form))
    ((letrec-p form) (inlined-letrec env form))
    ((with-args-p form) (inlined-with-args env form))
    ((loop-p form) (inlined-loop env form))
    ((setreg-p form) (inlined-setreg env form))
    ((apply-p form) (inlined-apply env form))
    (t (error "The value ~S is an invalid form." form))))

(defun inlined-literal (form)
  form)

(defun inlined-reference (form)
  form)

(defun inlined-sub (env form)
  (let ((expr1 (sub-expr1 form))
        (expr2 (sub-expr2 form)))
    (let ((expr1% (inlined% env expr1))
          (expr2% (inlined% env expr2)))
      `(- ,expr1% ,expr2%))))

(defun inlined-ifeq (env form)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (let ((lhs1 (inlined% env lhs))
          (rhs1 (inlined% env rhs))
          (then1 (inlined% env then))
          (else1 (inlined% env else)))
      `(if (= ,lhs1 ,rhs1)
           ,then1
           ,else1))))

(defun inlined-let (env form)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (let ((expr1 (inlined% env expr))
          (body1 (inlined% env body)))
      `(let ((,var ,expr1))
         ,body1))))

(defun inlined-letrec (env form)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (let ((env1 (inlined-environment-add name args expr env)))
      (let ((expr1 (inlined% env expr)) ; use ENV not inlining self-recursion
            (body1 (inlined% env1 body)))
        `(let ((,name ,args ,expr1))
           ,body1)))))

(defun inlined-with-args (env form)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (let ((body1 (inlined% env body)))
      `(with-args ,args
         ,body1))))

(defun inlined-loop (env form)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (let ((body1 (inlined% env body)))
      `(loop ,times
         ,body1))))

(defun inlined-setreg (env form)
  (let ((reg (setreg-reg form))
        (expr (setreg-expr form)))
    (let ((expr1 (inlined% env expr)))
      `(setreg ,reg ,expr1))))

(defun inlined-apply (env form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (let ((inliner (inlined-environment-lookup operator env)))
      (if inliner
          (destructuring-bind (args body) inliner
            (unless (= (length args) (length operands))
              (error "Invalid number of arguments: ~S~%" (length operands)))
            (inlined-apply% env args operands body))
          `(,operator ,@operands)))))

(defun inlined-apply% (env args operands body)
  (if args
      (destructuring-bind (arg . args1) args
        (destructuring-bind (operand . operands1) operands
          (let ((operand1 (inlined% env operand)))
            `(let ((,arg ,operand1))
               ,(inlined-apply% env args1 operands1 body)))))
      body))


;;;
;;; Dead code elimination
;;;

(defun alive-variable-p (var expr)
  (and (member var (flatten-list expr))
       t))

(defun alive-function-p (name expr)
  (alive-variable-p name expr))

(defun side-effect-p (form)
  (cond
    ((literal-p form) nil)
    ((reference-p form) nil)
    ((sub-p form) nil)
    ((ifeq-p form) (side-effect-p-ifeq form))
    ((let-p form) (side-effect-p-let form))
    ((letrec-p form) (side-effect-p-letrec form))
    ((with-args-p form) (side-effect-p-with-args form))
    ((loop-p form) (side-effect-p-loop form))
    ((setreg-p form) t)
    ((apply-p form) t)
    (t (error "The value ~S is an invalid form." form))))

(defun side-effect-p-ifeq (form)
  (let ((then (ifeq-then form))
        (else (ifeq-else form)))
    (or (side-effect-p then)
        (side-effect-p else))))

(defun side-effect-p-let (form)
  (let ((expr (let-expr form))
        (body (let-body form)))
    (or (side-effect-p expr)
        (side-effect-p body))))

(defun side-effect-p-letrec (form)
  (let ((body (letrec-body form)))
    (side-effect-p body)))

(defun side-effect-p-with-args (form)
  (let ((body (with-args-body form)))
    (side-effect-p body)))

(defun side-effect-p-loop (form)
  (let ((body (loop-body form)))
    (side-effect-p body)))

(defun elim (form)
  (cond
    ((literal-p form) (elim-literal form))
    ((reference-p form) (elim-reference form))
    ((sub-p form) (elim-sub form))
    ((ifeq-p form) (elim-ifeq form))
    ((let-p form) (elim-let form))
    ((letrec-p form) (elim-letrec form))
    ((with-args-p form) (elim-with-args form))
    ((loop-p form) (elim-loop form))
    ((setreg-p form) (elim-setreg form))
    ((apply-p form) (elim-apply form))
    (t (error "The value ~S is an invalid form." form))))

(defun elim-literal (form)
  form)

(defun elim-reference (form)
  form)

(defun elim-sub (form)
  form)

(defun elim-ifeq (form)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (let ((then1 (elim then))
          (else1 (elim else)))
      `(if (= ,lhs ,rhs) ,then1 ,else1))))

(defun elim-let (form)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (let ((expr1 (elim expr))
          (body1 (elim body)))
      (if (or (side-effect-p expr1)
              (alive-variable-p var body1))
          `(let ((,var ,expr1))
             ,body1)
          body1))))

(defun elim-letrec (form)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (let ((expr1 (elim expr))
          (body1 (elim body)))
      (if (alive-function-p name body1)
          `(let ((,name ,args ,expr1))
             ,body1)
          body1))))

(defun elim-with-args (form)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (let ((body1 (elim body)))
      `(with-args ,args
         ,body1))))

(defun elim-loop (form)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (let ((body1 (elim body)))
      `(loop ,times
          ,body1))))

(defun elim-setreg (form)
  form)

(defun elim-apply (form)
  form)


;;;
;;; Closure conversion
;;;

(defun closure (form)
  (multiple-value-bind (form1 fundefs) (closure% form nil)
    (closure-aux form1 fundefs)))

(defun closure-aux (form fundefs)
  (if fundefs
      (let ((fundef (car fundefs))
            (rest (cdr fundefs)))
        (destructuring-bind (name args expr) fundef
          `(let ((,name ,args ,expr))
             ,(closure-aux form rest))))
      form))

(defun closure% (form fundefs)
  (cond
    ((literal-p form) (closure-literal form fundefs))
    ((reference-p form) (closure-reference form fundefs))
    ((sub-p form) (closure-sub form fundefs))
    ((ifeq-p form) (closure-ifeq form fundefs))
    ((let-p form) (closure-let form fundefs))
    ((letrec-p form) (closure-letrec form fundefs))
    ((with-args-p form) (closure-with-args form fundefs))
    ((loop-p form) (closure-loop form fundefs))
    ((setreg-p form) (closure-setreg form fundefs))
    ((apply-p form) (closure-apply form fundefs))
    (t (error "The value ~S is an invalid form." form))))

(defun closure-literal (form fundefs)
  (values form fundefs))

(defun closure-reference (form fundefs)
  (values form fundefs))

(defun closure-sub (form fundefs)
  (values form fundefs))

(defun closure-ifeq (form fundefs)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (multiple-value-bind (then1 fundefs1) (closure% then fundefs)
      (multiple-value-bind (else1 fundefs2) (closure% else fundefs1)
        (values `(if (= ,lhs ,rhs) ,then1 ,else1) fundefs2)))))

(defun closure-let (form fundefs)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (multiple-value-bind (expr1 fundefs1) (closure% expr fundefs)
      (multiple-value-bind (body1 fundefs2) (closure% body fundefs1)
        (values `(let ((,var ,expr1))
                   ,body1)
                fundefs2)))))

(defun closure-letrec (form fundefs)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (multiple-value-bind (expr1 fundefs1) (closure% expr fundefs)
      (let ((fundefs2 (cons (list name args expr1) fundefs1)))
        (multiple-value-bind (body1 fundefs3) (closure% body fundefs2)
          (values body1 fundefs3))))))

(defun closure-with-args (form fundefs)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (multiple-value-bind (body1 fundefs1) (closure% body fundefs)
      (values `(with-args ,args ,body1)
              fundefs1))))

(defun closure-loop (form fundefs)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (multiple-value-bind (body1 fundefs1) (closure% body fundefs)
      (values `(loop ,times ,body1)
              fundefs1))))

(defun closure-setreg (form fundefs)
  (values form fundefs))

(defun closure-apply (form fundefs)
  (values form fundefs))


;;;
;;; Virtual machine code generation
;;;

(defun virtual (form)
  (cond
    ((literal-p form) (virtual-literal form))
    ((reference-p form) (virtual-reference form))
    ((sub-p form) (virtual-sub form))
    ((ifeq-p form) (virtual-ifeq form))
    ((let-p form) (virtual-let form))
    ((letrec-p form) (virtual-letrec form))
    ((with-args-p form) (virtual-with-args form))
    ((loop-p form) (virtual-loop form))
    ((setreg-p form) (virtual-setreg form))
    ((apply-p form) (virtual-apply form))
    (t (error "The value ~S is an invalid form." form))))

(defun virtual-literal (form)
  `(set ,form))

(defun virtual-reference (form)
  `(mov ,form))

(defun virtual-sub (form)
  (let ((expr1 (sub-expr1 form))
        (expr2 (sub-expr2 form)))
    (assert (pic-symbol-p expr1))
    (assert (pic-symbol-p expr2))
    `(sub ,expr1 ,expr2)))

(defun virtual-ifeq (form)
  (let ((lhs (ifeq-lhs form))
        (rhs (ifeq-rhs form))
        (then (ifeq-then form))
        (else (ifeq-else form)))
    (assert (pic-symbol-p lhs))
    (assert (pic-symbol-p rhs))
    (let ((then1 (virtual then))
          (else1 (virtual else)))
      `(ifeq ,lhs ,rhs ,then1 ,else1))))

(defun virtual-let (form)
  (let ((var (let-var form))
        (expr (let-expr form))
        (body (let-body form)))
    (let ((expr1 (virtual expr))
          (body1 (virtual body)))
      `(let ((,var ,expr1))
         ,body1))))

(defun virtual-letrec (form)
  (let ((name (letrec-name form))
        (args (letrec-args form))
        (expr (letrec-expr form))
        (body (letrec-body form)))
    (let ((expr1 (virtual expr))
          (body1 (virtual body)))
      `(let ((,name ,args ,expr1))
         ,body1))))

(defun virtual-with-args (form)
  (let ((args (with-args-args form))
        (body (with-args-body form)))
    (let ((iregs (input-regs (length args))))
      (virtual-with-args% args iregs body))))

(defun virtual-loop (form)
  (let ((times (loop-times form))
        (body (loop-body form)))
    (assert (pic-symbol-p times))
    (let ((body1 (virtual body)))
      `(loop ,times ,body1))))

(defun virtual-with-args% (args iregs body)
  (if args
      (destructuring-bind (arg . args1) args
        (destructuring-bind (ireg . iregs1) iregs
          `(let ((,arg (mov ,ireg)))
             ,(virtual-with-args% args1 iregs1 body))))
      (virtual body)))

(defun virtual-setreg (form)
  (let ((reg (setreg-reg form))
        (expr (setreg-expr form)))
    (assert (pic-symbol-p expr))
    `(setreg ,reg ,expr)))

(defun virtual-apply (form)
  (let ((operator (apply-operator form))
        (operands (apply-operands form)))
    (assert (every #'pic-symbol-p operands))
    `(call ,operator ,@operands)))


;;;
;;; Immediates optimization
;;;

(defun empty-immediates-environment ()
  nil)

(defun immediates-environment-add (var literal env)
  (acons var literal env))

(defun immediates-environment-lookup (var env)
  (cdr (assoc var env)))

(defun immediates (env inst)
  (cond
    ((let-inst-p inst) (immediates-let env inst))
    ((letrec-inst-p inst) (immediates-letrec env inst))
    ((set-inst-p inst) (immediates-set inst))
    ((mov-inst-p inst) (immediates-mov env inst))
    ((sub-inst-p inst) (immediates-sub env inst))
    ((ifeq-inst-p inst) (immediates-ifeq env inst))
    ((loop-inst-p inst) (immediates-loop env inst))
    ((setreg-inst-p inst) (immediates-setreg env inst))
    ((call-inst-p inst) (immediates-call env inst))
    (t (error "The value ~S is an invalid instruction." inst))))

(defun immediates-let (env inst)
  (let ((var (let-inst-var inst))
        (expr (let-inst-expr inst))
        (body (let-inst-body inst)))
    (if (set-inst-p expr)
        (let* ((literal (set-inst-literal expr))
               (env1 (immediates-environment-add var literal env)))
          (let ((body1 (immediates env1 body)))
            (if (alive-variable-p var body1)
                `(let ((,var ,expr))
                   ,body1)
                body1)))
        (let ((expr1 (immediates env expr))
              (body1 (immediates env body)))
          `(let ((,var ,expr1))
             ,body1)))))

(defun immediates-letrec (env inst)
  (let ((name (letrec-inst-name inst))
        (args (letrec-inst-args inst))
        (expr (letrec-inst-expr inst))
        (body (letrec-inst-body inst)))
    (let* ((env0 (empty-immediates-environment))
           (expr1 (immediates env0 expr)))
      (let ((body1 (immediates env body)))
        `(let ((,name ,args ,expr1))
           ,body1)))))

(defun immediates-set (inst)
  inst)

(defun immediates-mov (env inst)
  (let ((reg (mov-inst-reg inst)))
    (let ((literal (immediates-environment-lookup reg env)))
      (if literal
          `(set ,literal)
          `(mov ,reg)))))

(defun immediates-sub (env inst)
  (let ((expr1 (sub-inst-expr1 inst))
        (expr2 (sub-inst-expr2 inst)))
    (let ((expr2% (or (immediates-environment-lookup expr2 env)
                      expr2)))
      `(sub ,expr1 ,expr2%))))

(defun immediates-ifeq (env inst)
  (let ((lhs (ifeq-inst-lhs inst))
        (rhs (ifeq-inst-rhs inst))
        (then (ifeq-inst-then inst))
        (else (ifeq-inst-else inst)))
    (let ((rhs1 (or (immediates-environment-lookup rhs env)
                    rhs))
          (then1 (immediates env then))
          (else1 (immediates env else)))
      `(ifeq ,lhs ,rhs1 ,then1 ,else1))))

(defun immediates-loop (env inst)
  (let ((var (loop-inst-var inst))
        (body (loop-inst-body inst)))
    (let ((body1 (immediates env body)))
      `(loop ,var ,body1))))

(defun immediates-setreg (env inst)
  (let ((reg (setreg-inst-reg inst))
        (expr (setreg-inst-expr inst)))
    (let ((expr1 (or (immediates-environment-lookup expr env)
                     expr)))
      `(setreg ,reg ,expr1))))

(defun immediates-call (env inst)
  (let ((name (call-inst-name inst))
        (operands (call-inst-operands inst)))
    (let ((operands1 (mapcar #'(lambda (operand)
                                 (or (immediates-environment-lookup operand env)
                                     operand))
                             operands)))
      `(call ,name ,@operands1))))


;;;
;;; Register assignment
;;;

(defun input-regs (n)
  (loop for i from 0 below n
     collect
       (make-keyword (format nil "I~A" i))))

(defun input-reg-p (object)
  (and (pic-reg-p object)
       (member object (input-regs 8))
       t))

(defun local-regs (n)
  (loop for i from 0 below n
     collect
       (make-keyword (format nil "L~A" i))))

(defun local-reg-p (object)
  (and (pic-reg-p object)
       (member object (local-regs 8))
       t))

(defun empty-register-environment ()
  nil)

(defun register-environment-plist (env)
  (loop for lreg in (local-regs 8)
     append
       (let ((var (caar (member lreg env :key #'cdr))))
         (list lreg var))))

(defun register-environment-assign (var cont env)
  (if (register-environment-exists-p var env)
      env
      (let ((reg (register-environment-input-register var cont)))
        (if reg
            (register-environment-assign% reg var env)
            (let ((reg (register-environment-empty-register env)))
              (if reg
                  (register-environment-assign% reg var env)
                  (let ((reg (register-environment-dead-register cont env)))
                    (if reg
                        (register-environment-assign% reg var env)
                        (error "There is no available register.")))))))))

(defun register-environment-assign% (reg var env)
  (acons var reg
    (remove reg env :key #'cdr)))

(defun register-environment-input-register (var cont)
  (let ((cont1 (car cont)))
    (multiple-value-bind (iregs call-found-p cont-p)
        (register-environment-input-register% var nil cont1)
      (declare (ignore call-found-p))
      (if (and (singlep iregs) (not cont-p))
          (car iregs)
          nil))))

(defun register-environment-input-register% (var cont form)
  (assert (not (letrec-inst-p form)))
  (cond
    ((let-inst-p form) (register-environment-input-register-let var cont form))
    ((set-inst-p form) (register-environment-input-register-default))
    ((mov-inst-p form) (register-environment-input-register-default))
    ((sub-inst-p form) (register-environment-input-register-default))
    ((ifeq-inst-p form) (register-environment-input-register-ifeq var cont form))
    ((loop-inst-p form) (register-environment-input-register-loop var cont form))
    ((setreg-inst-p form) (register-environment-input-register-default))
    ((call-inst-p form) (register-environment-input-register-call var cont form))
    (t (error "The value ~S is an invalid form." form))))

(defun register-environment-input-register-let (var cont form)
  (let ((expr (let-inst-expr form))
        (body (let-inst-body form)))
    (let ((cont1 (cons body cont)))
      (multiple-value-bind (iregs1 call-found-p1 cont-p1)
          (register-environment-input-register% var cont1 expr)
        (if call-found-p1
            (values iregs1 t cont-p1)
            (register-environment-input-register% var cont body))))))

(defun register-environment-input-register-ifeq (var cont form)
  (let ((then (ifeq-inst-then form))
        (else (ifeq-inst-else form)))
    (multiple-value-bind (iregs1 call-found-p1 cont-p1)
        (register-environment-input-register% var cont then)
      (multiple-value-bind (iregs2 call-found-p2 cont-p2)
          (register-environment-input-register% var cont else)
        (values (union iregs1 iregs2)
                (or call-found-p1 call-found-p2)
                (or cont-p1 cont-p2))))))

(defun register-environment-input-register-loop (var cont form)
  (let ((body (loop-inst-body form)))
    (multiple-value-bind (iregs call-found-p cont-p)
        (register-environment-input-register% var cont body)
      (if iregs
          (values iregs call-found-p t)
          (values iregs call-found-p cont-p)))))

(defun register-environment-input-register-call (var cont form)
  (let ((operands (call-inst-operands form)))
    (let ((iregs (loop for operand in operands
                    for ireg in (input-regs (length operands))
                    if (eq var operand)
                    collect ireg))
          (cont-p (and (member var (flatten-list cont))
                       t)))
      (values iregs t cont-p))))

(defun register-environment-input-register-default ()
  (values nil nil nil))

(defun register-environment-empty-register (env)
  (let ((env1 (register-environment-plist env)))
    (loop for (reg var) on env1 by #'cddr
       if (null var)
       return reg)))

(defun register-environment-dead-register (cont env)
  (let ((env1 (register-environment-plist env)))
    (loop for (reg var) on env1 by #'cddr
       if (not (member var (flatten-list cont)))
       return reg)))

(defun register-environment-alive-registers (cont env)
  (let ((env1 (register-environment-plist env)))
    (loop for (reg var) on env1 by #'cddr
       if (member var (flatten-list cont))
       collect reg)))

(defun register-environment-assign-list (vars cont env)
  (if vars
      (destructuring-bind (var . rest) vars
        (register-environment-assign-list rest cont
         (register-environment-assign var cont env)))
      env))

(defun register-environment-exists-p (var env)
  (and (cdr (assoc var env))
       t))

(defun register-environment-lookup (var env)
  (if (pic-reg-p var)
      var
      (or (cdr (assoc var env))
          (error "The variable ~S not found." var))))

(defun assign (env cont inst)
  (cond
    ((let-inst-p inst) (assign-let env cont inst))
    ((letrec-inst-p inst) (assign-letrec env cont inst))
    ((set-inst-p inst) (assign-set env inst))
    ((mov-inst-p inst) (assign-mov env inst))
    ((sub-inst-p inst) (assign-sub env inst))
    ((ifeq-inst-p inst) (assign-ifeq env cont inst))
    ((loop-inst-p inst) (assign-loop env cont inst))
    ((setreg-inst-p inst) (assign-setreg env inst))
    ((call-inst-p inst) (assign-call env cont inst))
    (t (error "The value ~S is an invalid instruction." inst))))

(defun assign-let (env cont inst)
  (let ((var (let-inst-var inst))
        (expr (let-inst-expr inst))
        (body (let-inst-body inst)))
    (let ((cont1 (cons body cont)))
      (multiple-value-bind (expr1 env1) (assign env cont1 expr)
        (let* ((env2 (register-environment-assign var cont1 env1))
               (var1 (register-environment-lookup var env2)))
          (multiple-value-bind (body1 env3) (assign env2 cont body)
            (values `(let ((,var1 ,expr1))
                       ,body1)
                    env3)))))))

(defun assign-letrec (env cont inst)
  (assert (null cont))
  (let ((name (letrec-inst-name inst))
        (args (letrec-inst-args inst))
        (expr (letrec-inst-expr inst))
        (body (letrec-inst-body inst)))
    (let* ((cont1 (list expr))
           (env0 (register-environment-assign-list args cont1
                  (empty-register-environment)))
           (args1 (mapcar #'(lambda (arg)
                              (register-environment-lookup arg env0))
                          args))
           (expr1 (assign env0 nil expr)))
      (multiple-value-bind (body1 env1) (assign env cont body)
        (values `(let ((,name ,args1 ,expr1))
                   ,body1)
                env1)))))

(defun assign-set (env inst)
  (values inst env))

(defun assign-mov (env inst)
  (let ((reg (mov-inst-reg inst)))
    (let ((reg1 (register-environment-lookup reg env)))
      (values `(mov ,reg1) env))))

(defun assign-sub (env inst)
  (let ((expr1 (sub-inst-expr1 inst))
        (expr2 (sub-inst-expr2 inst)))
    (let ((expr1% (register-environment-lookup expr1 env))
          (expr2% (if (literal-p expr2)
                      expr2
                      (register-environment-lookup expr2 env))))
      (values `(sub ,expr1% ,expr2%)
              env))))

(defun assign-ifeq (env cont inst)
  (let ((lhs (ifeq-inst-lhs inst))
        (rhs (ifeq-inst-rhs inst))
        (then (ifeq-inst-then inst))
        (else (ifeq-inst-else inst)))
    (let ((lhs1 (register-environment-lookup lhs env))
          (rhs1 (if (literal-p rhs)
                    rhs
                    (register-environment-lookup rhs env))))
      (multiple-value-bind (then1 env1) (assign env cont then)
        (multiple-value-bind (else1 env2) (assign env1 cont else)
          (values `(ifeq ,lhs1 ,rhs1 ,then1 ,else1) env2))))))

(defun assign-loop (env cont inst)
  (let ((var (loop-inst-var inst))
        (body (loop-inst-body inst)))
    (let ((var1 (register-environment-lookup var env))
          ;; add loop continuation for keeping the loop register alive
          (cont1 (cons `(loop ,var) cont)))
      (multiple-value-bind (body1 env1) (assign env cont1 body)
        (values `(loop ,var1 ,body1) env1)))))

(defun assign-setreg (env inst)
  (let ((reg (setreg-inst-reg inst))
        (expr (setreg-inst-expr inst)))
    (let ((expr1 (if (literal-p expr)
                     expr
                     (register-environment-lookup expr env))))
      (values `(setreg ,reg ,expr1)
              env))))

(defun assign-call (env cont inst)
  (let ((name (call-inst-name inst))
        (operands (call-inst-operands inst)))
    (let ((operands1 (mapcar #'(lambda (operand)
                                 (if (literal-p operand)
                                     operand
                                     (register-environment-lookup operand env)))
                             operands)))
      (let ((alive-regs (register-environment-alive-registers cont env)))
        (if alive-regs
            (values `(with-save ,alive-regs
                       (call ,name ,@operands1))
                    env)
            (values `(call ,name ,@operands1)
                    env))))))


;;;
;;; Emit 8-bit PIC assembly
;;;

(defvar *label-counter* 0)

(defun genlbl (&rest things)
  (let ((cnt (princ-to-string *label-counter*)))
    (if (singlep things)
        (prog1 (symbolicate (car things) cnt)
          (incf *label-counter*))
        (prog1 (mapcar #'(lambda (thing)
                           (symbolicate thing cnt))
                       things)
          (incf *label-counter*)))))

(defun emit (dest inst)
  (cond
    ((let-inst-p inst) (emit-let dest inst))
    ((letrec-inst-p inst) (emit-letrec dest inst))
    ((set-inst-p inst) (emit-set dest inst))
    ((mov-inst-p inst) (emit-mov dest inst))
    ((sub-inst-p inst) (emit-sub dest inst))
    ((ifeq-inst-p inst) (emit-ifeq dest inst))
    ((loop-inst-p inst) (emit-loop dest inst))
    ((setreg-p inst) (emit-setreg dest inst))
    ((call-inst-p inst) (emit-call dest inst))
    ((with-save-inst-p inst) (emit-with-save dest inst))
    (t (error "The value ~S is an invalid instruction." inst))))    

(defun emit-let (dest inst)
  (let ((var (let-inst-var inst))
        (expr (let-inst-expr inst))
        (body (let-inst-body inst)))
    (append (emit `(:non-tail ,var) expr)
            (emit dest body))))

(defun emit-letrec (dest inst)
  (assert (eq dest :tail))
  (let ((name (letrec-inst-name inst))
        (args (letrec-inst-args inst))
        (expr (letrec-inst-expr inst))
        (body (letrec-inst-body inst)))
    (let ((args1 (loop for arg in args
                       for ireg in (input-regs (length args))
                    append (if (input-reg-p arg)
                               nil
                               `((movf ,ireg :w)
                                 (movwf ,arg)))))
          (expr1 (emit :tail expr))
          (body1 (emit dest body)))
      `(,@body1
        ,name
        ,@args1
        ,@expr1))))

(defun emit-set (dest inst)
  (let ((literal (set-inst-literal inst)))
    (cl-pattern:match dest
      ((:non-tail reg)
       (cond
         ((eq :null reg) nil)
         ((= literal 0) `((clrf ,reg)))
         (t `((movlw ,literal)
              (movwf ,reg)))))
      (:tail `((retlw ,literal)))
      (_ (error "The value ~S is an invalid destination." dest)))))

(defun emit-mov (dest inst)
  (let ((reg (mov-inst-reg inst)))
    (cl-pattern:match dest
      ((:non-tial :null) nil)
      ((:non-tail reg1) `((movf ,reg :w)
                          (movwf ,reg1)))
      (:tail `((movf ,reg :w)
               (return)))
      (_ (error "The value ~S is an invalid destination." dest)))))

(defun emit-sub (dest inst)
  (let ((expr1 (sub-inst-expr1 inst))
        (expr2 (sub-inst-expr2 inst)))
    (let ((expr2-insts (if (literal-p expr2)
                           `((movlw ,expr2))
                           `((movf ,expr2 :w)))))
      (cl-pattern:match dest
        ((:non-tail :null) nil)
        ((:non-tail reg) `(,@expr2-insts
                           (subwf ,expr1 :w)
                           (movwf ,reg)))
        (:tail `(,@expr2-insts
                 (subwf ,expr1 :w)
                 (return)))
        (_ (error "The value ~S is an invalid destination." dest))))))

(defun emit-ifeq (dest inst)
  (let ((lhs (ifeq-inst-lhs inst))
        (rhs (ifeq-inst-rhs inst))
        (then (ifeq-inst-then inst))
        (else (ifeq-inst-else inst)))
    (let ((rhs-insts (if (literal-p rhs)
                         `((movlw ,rhs))
                         `((movf ,rhs :w)))))
      (let ((then1 (emit dest then))
            (else1 (emit dest else)))
        (destructuring-bind (else-lbl end-lbl) (genlbl "ELSE" "END")
          (cl-pattern:match dest
            ((:non-tail _) `(,@rhs-insts
                             (subwf ,lhs :w)
                             (btfsc :status :z)
                             (goto ,else-lbl)
                             ,@then1
                             (goto ,end-lbl)
                             ,else-lbl
                             ,@else1
                             ,end-lbl))
            (:tail `(,@rhs-insts
                     (subwf ,lhs :w)
                     (btfsc :status :z)
                     (goto ,else-lbl)
                     ,@then1
                     ,else-lbl
                     ,@else1))
            (_ (error "The value ~S is an invalid destination." dest))))))))

(defun emit-loop (dest inst)
  (let ((reg (loop-inst-var inst))
        (body (loop-inst-body inst)))
    (let ((loop-lbl (genlbl "LOOP"))
          (body-insts (emit '(:non-tail :null) body)))
      (cl-pattern:match dest
        ((:non-tail _) `(,loop-lbl
                         ,@body-insts
                         (decfsz ,reg :f)
                         (goto ,loop-lbl)))
        (:tail `(,loop-lbl
                 ,@body-insts
                 (decfsz ,reg :f)
                 (goto ,loop-lbl)
                 (retlw 0)))
        (_ (error "The value ~S is an invalid destination." dest))))))

(defun emit-setreg (dest inst)
  (let ((reg (setreg-inst-reg inst))
        (expr (setreg-inst-expr inst)))
    (let ((expr-insts (if (literal-p expr)
                          (if (= expr 0)
                              `((clrf ,reg))
                              `((movlw ,expr)
                                (movwf ,reg)))
                          `((movf ,expr :w)
                            (movwf ,reg)))))
      (cl-pattern:match dest
        ((:non-tail _) `(,@expr-insts))
        (:tail `(,@expr-insts
                 (return)))
        (_ (error "The value ~S is an invalid destination." dest))))))

(defun emit-call (dest inst)
  (let ((name (call-inst-name inst))
        (operands (call-inst-operands inst)))
    (let ((ireg-insts (loop for operand in operands
                            for ireg in (input-regs (length operands))
                         append (cond
                                  ((literal-p operand) `((movlw ,operand)
                                                         (movwf ,ireg)))
                                  ((input-reg-p operand) nil)
                                  (t `((movf ,operand :w)
                                       (movwf ,ireg)))))))
      (cl-pattern:match dest
        ((:non-tail :null) `(,@ireg-insts
                             (call ,name)))
        ((:non-tail reg) `(,@ireg-insts
                           (call ,name)
                           (movwf ,reg)))
        (:tail `(,@ireg-insts
                 (goto ,name)))
        (_ (error "The value ~S is an invalid destination." dest))))))

(defun emit-with-save (dest inst)
  (let ((regs (with-save-inst-regs inst))
        (expr (with-save-inst-expr inst)))
    (assert (call-inst-p expr))
    (let ((save-insts (loop for reg in regs
                         append `((movf ,reg :w)
                                  (call push-stack))))
          (call-insts (emit-call dest expr))
          (restore-insts (loop for reg in regs
                            append `((call pop-stack)
                                     (movwf ,reg)))))
      `(,@save-insts
        ,@call-insts
        ,@restore-insts))))


;;;
;;; Output 8-bit PIC assembly
;;;

(defun output (insts)
  (loop for inst in insts
     do (if (listp inst)
            (output-inst inst)
            (output-label inst))))

(defun output-inst (inst)
  (let ((operator (car inst))
        (operands (cdr inst)))
    (let ((operands1 (mapcar #'compile-token operands)))
      (format t "~8T~A~16T~{~A~^,~}~%" operator operands1))))

(defun output-label (label)
  (format t "~A~%" (compile-token label)))


;;;
;;; Compilation
;;;

(defun repeatedly (limit fun form)
  (if (= limit 0)
      form
      (let ((form1 (funcall fun form)))
        (if (equal form form1)
            form1
            (repeatedly (1- limit) fun form1)))))

(defvar *limit* 1000)

(defun compile-pic (form fundefs)
  (output
   (emit :tail
    (assign (empty-register-environment) nil
     (immediates (empty-immediates-environment)
      (virtual
       (closure
        (repeatedly *limit*
          #'(lambda (form)
              (elim
               (flatten
                (beta (empty-beta-environment) form))))
          (alpha2 (empty-alpha2-environment)
           (alpha1 (empty-alpha1-environment)
            (k-normal (empty-k-normal-environment)
             (repeatedly *limit*
               #'(lambda (form)
                   (inlined fundefs form))
               (expand form)))))))))))))

(defun expand-pic (form)
  (expand form))

(defun inlined-pic (form fundefs)
  (repeatedly *limit*
    #'(lambda (form)
        (inlined fundefs form))
    (expand-pic form)))

(defun k-normal-pic (form fundefs)
  (k-normal (empty-k-normal-environment)
   (inlined-pic form fundefs)))

(defun alpha-pic (form fundefs)
  (alpha2 (empty-alpha2-environment)
   (alpha1 (empty-alpha1-environment)
     (k-normal-pic form fundefs))))

(defun beta-pic (form fundefs)
  (beta (empty-beta-environment)
   (alpha-pic form fundefs)))

(defun flatten-pic (form fundefs)
  (flatten
   (beta-pic form fundefs)))

(defun elim-pic (form fundefs)
  (elim
   (inlined-pic form fundefs)))

(defun closure-pic (form fundefs)
  (closure
   (repeatedly *limit*
     #'(lambda (form)
         (elim
          (flatten
           (beta (empty-beta-environment) form))))
     (alpha-pic form fundefs))))

(defun virtual-pic (form fundefs)
  (virtual
   (closure-pic form fundefs)))

(defun immediates-pic (form fundefs)
  (immediates (empty-immediates-environment)
    (virtual-pic form fundefs)))

(defun assign-pic (form fundefs)
  (assign (empty-register-environment) nil
   (immediates-pic form fundefs)))

(defun emit-pic (form fundefs)
  (emit :tail
   (assign-pic form fundefs)))

(defun output-pic (form fundefs)
  (output
   (emit-pic form fundefs)))


;;;
;;; Program
;;;

(defun make-program ()
  (make-hash-table))

(defun program-defun (program name args body)
  (unless (pic-symbol-p name)
    (error "The value ~S is an invalid pic symbol." name))
  (dolist (arg args)
    (unless (pic-symbol-p arg)
      (error "The value ~S is an invalid pic symbol." arg)))
  (unless (equal args (remove-duplicates args))
    (error "The values ~S are invalid arguments." args))
  (when (eq name 'main)
    (unless (null args)
      (error "The main function must have no arguments.")))
  (when (eq name 'init)
    (unless (null args)
      (error "The init function must have no arguments.")))
  (when (eq name 'intr)
    (unless (null args)
      (error "The intrrupt function must have no arguments.")))
  (let* ((fundefs (program-fundefs-excluding program name))
         (insts (with-output-to-string (s)
                  (let ((*standard-output* s))
                    (cond
                      ((eq name 'main) (write-line "_MAIN")
                                       (compile-pic body fundefs))
                      ((eq name 'init) (write-line "_INIT")
                                       (compile-pic body fundefs))
                      ((eq name 'intr) (write-line "_INTR")
                                       (compile-pic body fundefs))
                      (t (write-line (compile-token name) s)
                         (compile-pic `(with-args ,args ,body) fundefs)))))))
    (setf (gethash name program) (list args body insts))
    name))

(defun program-defmacro (name expander)
  (setf (get name 'pic-macro) expander)
  name)

(defun program-names (program)
  (hash-table-keys program))

(defun program-fundefs (program)
  (loop for name in (program-names program)
     collect
       (destructuring-bind (args body insts) (program-by-name program name)
         (declare (ignore insts))
         (let ((body1 (expand body)))
           (list name args body1)))))

(defun program-fundefs-excluding (program name)
  (remove name (program-fundefs program) :key #'car))

(defun program-exists-p (program name)
  (and (gethash name program)
       t))

(defun program-by-name (program name)
  (or (values (gethash name program))
      (error "The function ~S not defined." name)))

(defun program-compute-dependency (program)
  (remove-duplicates
   (append
    (program-compute-dependency% program 'init)
    (program-compute-dependency% program 'main)
    (program-compute-dependency% program 'intr))))

(defun program-compute-dependency% (program name)
  (if (program-exists-p program name)
      (destructuring-bind (args body insts) (program-by-name program name)
        (declare (ignore args body))
        (remove-duplicates
         (loop for name1 in (program-names program)
            if (and (not (eq name name1))
                    (search (compile-token name1) insts))
            append (cons name1
                         (compute-dependency program name1)))))
      nil))

(defun program-macro-exists-p (name)
  (and (symbolp name)
       (get name 'pic-macro)
       t))

(defun program-macro (name)
  (if (program-macro-exists-p name)
      (get name 'pic-macro)
      (error "The macro ~S not defined." name)))

(defun program-disassemble (program name)
  (destructuring-bind (args body insts) (program-by-name program name)
    (declare (ignore args body))
    (princ insts)
    (values)))

(defun program-clear (program)
  (clrhash program)
  (values))

(defun program-main (program)
  (unless (program-exists-p program 'main)
    (error "The main function not defined."))
  (program-by-name program 'main))

(defun program-init (program)
  (values (gethash 'intr program)))

(defun program-intr (program)
  (values (gethash 'intr program)))

(defun program-compile (program &optional (stream *standard-output*))
  (program-main program)                ; Check main existing.
  (write-line "        INCLUDE\"p12f683.inc\"" stream)
  (write-line "        list p=12f683" stream)
  (write-line "" stream)
  (write-line "        __CONFIG _CP_OFF & _CPD_OFF & _WDT_OFF & _BOD_ON & _IESO_OFF& _PWRTE_ON & _INTOSCIO & _MCLRE_OFF" stream)
  (write-line "" stream)
  (write-line "        CBLOCK  020h" stream)
  (write-line "        L0,L1,L2,L3,L4,L5,L6,L7 ; local registers" stream)
  (write-line "        I0,I1,I2,I3,I4,I5,I6,I7 ; input registers" stream)
  (write-line "        NULL                    ; null register (for NOP)" stream)
  (write-line "        SP,STMP,STK             ; stack registers" stream)
  (write-line "        ENDC" stream)
  (write-line "" stream)
  (write-line "        ORG 0" stream)
  (write-line "        GOTO    MAIN" stream)
  (write-line "        ORG 4" stream)
  (write-line "        GOTO    INTR" stream)
  (output-functions program stream)
  (output-intr program stream)
  (output-main program stream)
  (write-line "_PUSH_STACK" stream)
  (write-line "        MOVWF   STMP" stream)
  (write-line "        MOVF    SP,W" stream)
  (write-line "        MOVWF   FSR" stream)
  (write-line "        MOVF    STMP,W" stream)
  (write-line "        MOVWF   INDF" stream)
  (write-line "        INCF    SP,F" stream)
  (write-line "        RETURN" stream)
  (write-line "_POP_STACK" stream)
  (write-line "        DECF    SP,F" stream)
  (write-line "        MOVF    SP,W" stream)
  (write-line "        MOVWF   FSR" stream)
  (write-line "        MOVF    INDF,W" stream)
  (write-line "        RETURN" stream)
  (write-line "INTR" stream)
  (write-line "        CALL    _INTR" stream)
  (write-line "        RETFIE" stream)
  (write-line "MAIN" stream)
  (write-line "        MOVLW   STK             ; initialize SP" stream)
  (write-line "        MOVWF   SP" stream)
  (write-line "        CALL    _INIT" stream)
  (write-line "        CALL    _MAIN" stream)
  (write-line "        END" stream)
  (values))

(defun output-main (program stream)
  (destructuring-bind (args body insts) (program-main program)
    (declare (ignore args body))
    (princ insts stream)))

(defun output-intr (program stream)
  (let ((intr (program-intr program)))
    (if intr
        (destructuring-bind (args body insts) intr
          (declare (ignore args body))
          (princ insts stream))
        (progn
          (write-line "_INTR" stream)
          (write-line "        RETURN" stream)))))

(defun output-functions (program stream)
  (loop for name in (program-compute-dependency program)
     do (destructuring-bind (args body insts)
            (program-by-name program name)
          (declare (ignore args body))
          (princ insts stream))))


;;;
;;; Programming Interfaces
;;;

(defvar *program* (make-program))

(defmacro defpic (name args body)
  `(program-defun *program* ',name ',args ',body))

(defmacro defpicmacro (name args &rest body)
  `(program-defmacro ',name #'(lambda ,args ,@body)))

(defun pic-compile (&optional (stream *standard-output*))
  (program-compile *program* stream))

(defun pic-disassemble (name)
  (program-disassemble *program* name))

(defun pic-macroexpand (form)
  (labels ((aux (form expanded-p)
             (multiple-value-bind (expansion newly-expanded-p)
                 (pic-macroexpand1 form)
               (if newly-expanded-p
                   (aux expansion t)
                   (values expansion expanded-p)))))
    (aux form nil)))

(defun pic-macroexpand1 (form)
  (if (macro-p form)
      (let ((expander (macro-expander form))
            (operands (macro-operands form)))
        (values (apply expander operands) t))
      (values form nil)))

(defun pic-clear ()
  (program-clear *program*))


;;;
;;; Macros
;;;

(defpicmacro progn (&rest forms)
  (labels ((aux (forms0)
             (let ((form (car forms0))
                   (rest (cdr forms0)))
               (if rest
                   `(let ((tmp ,form))
                      ,(aux rest))
                   form))))
    (aux forms)))

(defpicmacro nop ()
  '(setreg :null 0))

(defpicmacro setbank0 ()
  '(setreg :status #x20))

(defpicmacro setbank1 ()
  '(setreg :status #x00))
