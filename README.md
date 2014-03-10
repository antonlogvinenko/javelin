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
    * where to use?
    * redefine types
    * test on all classes, again
  * set up for functional testing, add a few tens of large classes
  * foundation for unit testing
  * what's next?

Javeling deferred tasks:
* need more unit testing for bytecode parser

Haskell deferred tasks:
* cabal sandboxing
* quickcheck/hunit/tasty/hspec
* emacs modes
* hlint
