(in-package #:lllvm)

;;;example usage and printing code

(module "test"
  (cl:let ((fun1 (define f1 (llvm:int-type 8) ()
                   (ret (add
                         (lllvm.const:int #t(lllvm.type:int 8) (wrap 2))
                         (lllvm.const:int #t(lllvm.type:int 8) (wrap 2)))))))
    (cl:print fun1)))


