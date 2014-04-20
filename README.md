Javelin
=======
JVM 8, interpreting JVM 8 spec implementation in Haskell.

*Long term*
* Bytecode parser [**Done**]
* Runtime data structures [**In progress**]
* Linking/loading
* Execution (everything but invokedynamic & verification)
* Garbage collection
* Bytecode verification, invokedynamic

*Short term: Runtime data structures*
* Record pattern matching syntax
* Memory access for instructions
* Chapters 3, 5 from JVM spec
* MVP: run a trivial main class
    * Write java Main class, compile, find out commands
    * Program commands
    * Program execution of commands
    * Write trivial class loading and what it takes to execute a static main method

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