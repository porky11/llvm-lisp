(in-package #:defpackage+-user-1)

(defpackage+ #:lllvm.const)

(defpackage+ #:lllvm.type)

(defpackage+ #:lisp-llvm
  (:nicknames #:lllvm)
  (:import-from #:cl
               #:defvar
               #:defstruct
               #:in-package

               #:defun #:&optional #:&rest #:&whole #:t
               
               #:defmacro
               #:define-compiler-macro
               
               #:format
               #:terpri

               #:defmethod #:print-object
               )
  (:export #:module
           #:basic-block
           #:declare
           #:define
           #:type-of
           #:function

           #:const

           #:print-value
           #:print-type
           #:print-module))
(defpackage+ #:llvm-lisp-user
  (:use #:lllvm))

(defmacro lllvm::define-reverse-macro-single (one two)
  `(define-compiler-macro ,one (&whole whole value)
       (if (and (consp value) (eq (car value) ',two))
           (cadr value)
           whole)))

(defmacro lllvm::define-reverse-macro-multi (one &rest rest)
  `(define-compiler-macro ,one (&whole whole value)
       (if (and (consp value) (member (car value) ',rest))
           (cadr value)
           whole)))

(defmacro lllvm::define-reverse-macro (one two)
  `(progn (lllvm::define-reverse-macro-single ,one ,two)
          (lllvm::define-reverse-macro-single ,two ,one)))

(in-package #:lllvm)


(defvar *module*)

(defvar *builder*)

(defvar *function*)

(defstruct (wrapper (:conc-name "UN")
                    (:constructor wrap (wrap)))
  wrap)

(defstruct (value (:include wrapper)
                  (:constructor wrap-value (wrap))))
(define-reverse-macro-single wrap-value unwrap)

(defstruct (module (:include wrapper)
             (:constructor wrap-module (wrap))))
(define-reverse-macro-single wrap-module unwrap)

(defstruct (type (:include wrapper)
                 (:constructor wrap-type (wrap))))
(define-reverse-macro-single wrap-type unwrap)

(define-reverse-macro-multi unwrap wrap-value wrap-module wrap-type)

(defun expand-builder-macro (name args)
  `(,name *builder* ,@args))

(cl:defmacro builder-macro (name &rest args)
  `(macrolet ((builder-macro (name &rest args)
                (expand-builder-macro name args)))
     (wrap-value ,(expand-builder-macro name args))))

(defun prin1-value (value &optional (stream t))
  (format stream "#v~S"
          (llvm::print-value-to-string value))
  value)

(defun prin1-module (value &optional (stream t))
  (format stream "#m~S"
          (llvm::print-module-to-string value))
  value)

(defun prin1-type (value  &optional (stream t))
  (format stream "#t~S"
          (llvm::print-type-to-string value))
  value)

(defun print-value (value &optional (stream t))
  (terpri)
  (prin1-value value stream))

(defun print-module (value &optional (stream t))
  (terpri)
  (prin1-module value stream))

(defun print-type (value  &optional (stream t))
  (terpri)
  (prin1-type value stream))


(defmethod print-object ((value value) stream)
  (prin1-value (unwrap value) stream))

(defmethod print-object ((value module) stream)
  (prin1-module (unwrap value) stream))

(defmethod print-object ((value type) stream)
  (prin1-type (unwrap value) stream))

               
          


(in-package #:cl-user)

(defmacro type-defmacro (name &rest others &aux (sname (symbol-name name)))
  (let ((nsym (intern sname :lllvm.type)))
    (export nsym :lllvm.type)
    `(progn
       (defmacro ,nsym (&rest args)
           `(,',(intern (format nil "~a-TYPE" name) :llvm) ,@args))
       ,(if others
            `(type-defmacro ,@others)))))

(type-defmacro array
               double
               element
               float
               fp128
               function
               int-pointer
               int
               int1
               int8
               int16
               int32
               int64
               label
               pointer
               ppc-fp128
               return
               struct
               vector
               void
               x86-fp80)

(labels ((type0 (args)
           (if (consp args)
               (destructuring-bind (type . rest) args
                 `(,(intern (symbol-name type) :lllvm.type) ,@(mapcar #'type0 rest)))
               args)))
  (defmacro lllvm::type (&rest args)
    `(wrap-type ,(type0 args)))
  (defmacro lllvm.const::type (&rest args)
    `(wrap-type ,(type0 args))))

(defmacro get-environment (&environment env)
  env)

(defmacro make-environment ()
  '(let ((x 1)
         (y 2)
         (z 4))
    (get-environment)))

(defmacro with-environment (env &body body)
  `(macrolet ((macro () (progn ,@body)))
     (macroexpand `(macro) ,env)))
  


(labels ((const0 (args)
           (if (consp args)
               (destructuring-bind (type . rest) args
                 `(,(intern (symbol-name type) :lllvm.const) ,@(mapcar #'const0 rest)))
               args)))
  (defmacro lllvm::const (&rest args)
    `(wrap-value ,(const0 args))))

(defun type-reader (stream &optional char subchar)
  (declare (ignore char subchar))
  (let ((*package* #.(find-package :lllvm.type)))
    `(lllvm::wrap-type ,(read stream t nil t))))

(set-dispatch-macro-character #\# #\t
                              #'type-reader)



(defmacro lllvm:module (name &body body)
  `(let ((lllvm::*module* (llvm:make-module ,name)))
     ,@body
     (lllvm::wrap-module lllvm::*module*)))

(defmacro lllvm:function (name ftype)
  `(lllvm::wrap-value (llvm:add-function lllvm::*module* ,(symbol-name name) (lllvm::unwrap ,ftype))))

(defmacro lllvm:declare (name type0 types)
  `(lllvm:function ,name
                   (lllvm::wrap-type (llvm:function-type ,type0 (list ,@types) ))))

(defmacro lllvm:basic-block (block-name &body body)
  `(llvm:with-objects ((lllvm::*builder* llvm:builder))
     (let ((,block-name (llvm:append-basic-block lllvm::*function* ,(symbol-name block-name))))
       (llvm:position-builder-at-end lllvm::*builder* ,block-name)
       ,@body
       (lllvm::wrap-value ,block-name))))

(defmacro lllvm:define (name type0 types &body body)
  `(let* ((lllvm::*function* (lllvm::unwrap (lllvm:declare ,name ,type0 ,types))))
     (lllvm:basic-block ,name ,@body)
     (unless (llvm:verify-function lllvm::*function*) (error "Function incomplete"))
     (lllvm::wrap-value lllvm::*function*)))

(defun lllvm:type-of (object)
  (lllvm::wrap-type (llvm:type-of (lllvm::unwrap object))))

(macrolet
    ((exports (&aux exports)
       (do-external-symbols (sym :llvm `(progn ,@exports))
         (let ((name (symbol-name sym)))
           (when (eql 0 (search "CONST-" name))
             (let ((nsym (intern (subseq name 6) :lllvm.const)))
               (export nsym :lllvm.const)
               (push
                `(progn
                   (defmacro ,nsym (&rest args)
                     `(lllvm::wrap-value
                       (,',sym ,@(mapcar (lambda (arg) `(lllvm::unwrap ,arg)) args)))))
                exports)))
           (when (eql 0 (search "BUILD-" name))
             (let ((nsym (intern (subseq name 6) :lllvm)))
               (export nsym :lllvm)
               (push
                `(progn
                   (defmacro ,nsym (&rest args)
                     `(lllvm::wrap-value
                       (,',sym lllvm::*builder*
                               ,@(mapcar (lambda (arg) `(lllvm::unwrap ,arg)) args)
                               ,,@(if (member nsym '(lllvm::ret lllvm::br lllvm::cond-br))
                                      nil (list (symbol-name nsym)))))))
                exports)))))))
  (exports))

(defmacro lllvm::if (if then &optional else)
  `(lllvm:cond-br ,if (lllvm:basic-block #:then ,then)
                       (lllvm:basic-block #:else ,else)))

(defmacro lllvm::loop (&body body &aux (block (gensym "block")))
  `(lllvm:basic-block ,block ,@body (lllvm:br ,block)))

(defun lllvm::funcall (function &rest args)
  (llvm:with-objects ((engine llvm:execution-engine lllvm::*module*))
    (let ((ptr (llvm:pointer-to-global engine (lllvm::unwrap function))))
      (if (cffi:pointer-eq (lllvm::unwrap function) ptr)
          (llvm:run-function engine ptr (mapcar #'lllvm::unwrap args))
          (error "not supp")))))


