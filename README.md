Javelin
=======
JVM 8, interpreting JVM 8 spec implementation in Haskell.

*Long term*
* Bytecode parser [**Done**]
* Runtime data structures [**In progress**]
* Linking/loading [** In progress**]
* Execution (everything but invokedynamic & verification)
* Garbage collection
* Bytecode verification, invokedynamic

*Short term: Runtime data structures*
* Rewrite Thread with Execution in Thread.hs, where Execution = { Runtime, Thread }
* execute function must optionally collect trace of execution in some way
* implement bootstrap method
* implement class loading (not linking and initialization for now)

* Arguments length: varying amount?
* implement Constants, loads, stores, math, conversions, comparisons, extended, reserved
* unsigned byte, other types operated indirectly
* conversion i2l, etc?
* implement Control, references
* implement return, exceptions, monitors, memory access

* Memory access for instructions
* DSL implementation for arguments and reading commands
* Reference type
* Chapters 3, 5 from JVM spec
* MVP: run a trivial main class
    * Write java Main class, compile, find out commands
    * Program commands
    * Program execution of commands
    * Write trivial class loading and what it takes to execute a static main method
* next: linking, general class loading, native api
    
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

Don't wanna lose this: http://dev.stephendiehl.com/hask/
And this http://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad