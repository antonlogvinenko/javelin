javelin
=======
Haskell JVM

Long term
* bytecode parser
* runtime
* linking
* garbage collection
* bytecode verification (optional)

Short term
* bytecode parser
  * deserialize
    * parse length in all attributes - it takes some bytes, for verification
    * overall structure
    * verification in place
    * unit testing, acceptantce testing
  * simple validation based on bytecode definition
  * modification: store higher level data about class file
  * tests
  
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
