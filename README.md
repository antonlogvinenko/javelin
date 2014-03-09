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
  * bug finder
    * gather a lot of classes
    * code to run on all files
    * fail on the first one
    * find errors, fix
  * set up for functional testing, add a few tens of large classes
  * descriptors string parsers
  * foundation for unit testing
  * refactor, rewrite with do-notation instead of applicative notation where appropriate
  * make a function to pretty print constant pool, entry per line, with indices
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
