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
  * overall structure
    * attributes
    * MethodHandleInfo, MethodTypeInfo, InvokeDynamicInfo
  * acceptance testing, unit testing
  * simple validation based on bytecode definition
  
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
