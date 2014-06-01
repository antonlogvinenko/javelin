Javelin
=======
JVM 8, interpreting JVM 8 spec implementation in Haskell.

*Long term*
* Bytecode parser [**Done**]
* Runtime data structures [**In progress**]
* Linking/loading [**In progress**]
* Execution (everything but invokedynamic & verification)
* Garbage collection
* Bytecode verification, invokedynamic

*Short term: Runtime data structures*
* init thread stack to run Main { public static void main(Sting...) }
* implement bootstrap method: loading, linking, initializing
* ad hoc loading of System, System.out to println data, all required instrucitons (milestone)

Next: collecting instructions
* execute function must optionally collect trace of execution in some way
 ** pass collecting function?
 ** use State monad on final instructions? (not in instruction DSL)
 ** how to write step with State monad?
 ** Should "state" result of executing Instruction monad contain description of action?
 ** Should "state" result of executing Instruciton implementation contain description?
* how to collect trace from other Threads?

Excluding for now:
* verification
* exceptions
* synchronization
* invokedynamic
* multiple threads (transactional access to memory)
* user derined class loaders (only boostrap)

* Memory access for instructions
* DSL implementation for arguments and reading commands
* Reference type
* MVP: run a trivial main class
    * Write java Main class, compile, find out commands
    * Program commands
    * Program execution of commands
    * Write trivial class loading and what it takes to execute a static main method

* Arguments length: varying amount?
* implement Constants, loads, stores, math, conversions, comparisons, extended, reserved
* unsigned byte, other types operated indirectly
* conversion i2l, etc?
* implement Control, references
* implement return, exceptions, monitors, memory access

*Javelin deferred tasks*
* need more unit testing for bytecode parser

*Possible offspring projects
* Viewing step by step bytecode execution interactively
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