javelin
=======
Haskell JVM

Long term
* Bytecode parser
* Runtime data structures
* Linking/loading
* Execution (everything but invokedynamic & verification)
* Garbage collection
* Bytecode verification, invokedynamic

Short term
* bytecode parser
  * descriptors string parsers
    * unit tests for descriptors/signatures
    * where to use them, redefine types (add, not replace)
    * run on all classes, again, find bugs
  * Main.hs - switch between research modes
  * running against 1K classes - make it a test

Javeling deferred tasks:
* need more unit testing for bytecode parser

Haskell deferred tasks:
* cabal sandboxing
* quickcheck/hunit/tasty/hspec
* emacs modes
* hlint
