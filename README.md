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
  * make parse a few more lcasses
  * set up functional tests
  * set up unit testing
  * add more complicated classes to functional testing, foundation for unit testing
  * refactor, rewrite with do-notation instead of applicative notation
  * what's next?

Deferred:
* need more unit testing for bytecode parser


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
