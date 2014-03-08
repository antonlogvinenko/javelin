javelin
=======
Haskell JVM

Long term
* bytecode parser
* runtime data structures
* linking/loading
* execution (everything but invokedynamic & verification)
* garbage collection
* bytecode verification, invokedynamic (optional)

Short term
* bytecode parser
  * fix attributes
  * make parse a few classes
  * set up functional tests
  * set up unit testing

Deferred:
* need more unit testing for bytecode parser


stackMapTableAttr
innerClassAttr
sourceDebugExtension
lineNumber
paramAnnsAttr
annDefaultAttr

Haskell:
* sandbox
* read, use quickcheck, hunit, tasty, hspec?, write tests
* test - functional (read file)
* infrastructure: inform where the error happened - EOF when parsing WHAT?
* parser functions
* read file
* emacs modes - more useful...
* use lenses for upd2bytes
* hlint
