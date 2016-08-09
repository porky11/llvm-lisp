(defsystem #:llvm-lisp
  :depends-on (#:defpackage-plus #:llvm)
  :description "Some simpler functions for using llvm with lisp (doesn't work, and maybe won't, but using a lisp language with llvm-like operations would be nice)"
  :license "WTFPL?"
  :components ((:file "llvm-tools")))

