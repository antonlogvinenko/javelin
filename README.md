javelin
=======

Haskell JVM
* bytecode parser
  * deserialize
    * parse descriptors/methods/signatures etc - use parsec here
    * parse numbers
    * parse length in all attributes - it takes some bytes, for verification
    * overall structure
    * change inheritance?
    * understand the whole structure, semantics
    * other details besides any kind of verification
    * verification in place
    * unit testing, acceptantce testing
  * validation, verification
    * using spec, 200 pages of verification...

  * modification: store higher level data about class file

  * test

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
