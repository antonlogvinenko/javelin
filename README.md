javelin
=======

Haskell JVM

* javelin-2
  * bytecode
    * write missing parsers
      * attrributes
      * parse length in all attributes - it takes some bytes, for verification
    * modules...
    * strings, number formats
    * overall structure
    * refactor parsers
      * common code for parsing tables
      * common code for getBytes n
    * change inheritance?
    * understand all structure, semantics
    * other details besides any kind of verification

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
