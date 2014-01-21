javelin
=======

Haskell JVM

* javelin-2
  * bytecode
    * strings
    * parse descriptors/methods/signatures etc
    * parse numbers
    * parse length in all attributes - it takes some bytes, for verification
    * overall structure
    * change inheritance?
    * understand the whole structure, semantics
    * other details besides any kind of verification
    * test

  * validation, verification
    * in place
    * using spec

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
