I would like to write low level code with lisp and llvm.
Adding some macros to the lispy llvm (for type inference, variable bindings, function overloading, method selection, automatic freeing memory) would make it easy to implement a language, that can be compiled (JIT or AOT), and adding completely new features to the language, like in lisps in general, but here with full control about the memory.
This is not really working yet, and I'm not sure if implementing this lisp in common lisp is a good idea.
in llvm-tools.lisp are the macros, in test.lll.lisp is some simple example, that should work
(maybe you have to download my fork of cl-llvm, implementing some printer-functons that are not in the original)
