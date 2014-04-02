javelin
=======
Haskell JVM

*Long term*
* Bytecode parser [**Done**]
* Runtime data structures [**In progress**]
* Linking/loading
* Execution (everything but invokedynamic & verification)
* Garbage collection
* Bytecode verification, invokedynamic

*Short term: Runtime data structures*
* Reread chapters 2, 3 from JVM spec
* Start defining data structures and logic reqiured
* Next?

*Javelin deferred tasks*
* need more unit testing for bytecode parser

*Possible offspring projects"
* UI for viewing bytecode
* Disassembler
* Decompiler?

*Haskell deferred tasks*
* cabal sandboxing
* quickcheck/hunit/tasty/hspec
* checkSpec, htest
* emacs modes
* hlint

*Java 7 -> 8 bytecode changes*
* 4.7.24, MethodParameters attribute (new)
* 4.7.20, RuntimeVisibleTypeAnnotations (new)
* 4.7.21, RuntimeInvisibleTypeAnnotations (new)
* Changes in descriptor and signatures terminology (refactoring)