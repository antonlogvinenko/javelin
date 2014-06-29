Javelin
=======
JVM 8, interpreting JVM 8 spec implementation in Haskell.

*Long term*
* Bytecode parser [**Done**]
* Runtime data structures [**Done**]
* Loading, linking, initializing: light version [**In progress**]
* Executing instructions
* All features skipped
 * verification
 * exceptions
 * synchronization
 * invokedynamic
 * multiple threads (use haskell threads and transactions for shared memory?)
 * user derined class loaders (only boostrap)

*Short term*
* Linking
 * preparation: static fields creation
 * get values from sym table - polymorphims? lots of duplicated code
 * start using either eveywhere and add "get bytecode" rt -> either e bc funs
 * errors, compose with loading errors?
 * who starts the whole lli cycle?
* Loading
  * bootstrap array load
  * fix: can be several class loaders for a class

* Resolution
* Initializing

* ad hoc loading of System, System.out to println data, all required instrucitons (milestone)
* init thread stack to run Main { public static void main(Sting...) }
* Instructions
 * Collecting
   * execute function must optionally collect trace of execution in some way
   * pass collecting function?
   * use State monad on final instructions? (not in instruction DSL)
   * how to write step with State monad?
   * Should "state" result of executing Instruction monad contain description of action?
   * Should "state" result of executing Instruciton implementation contain description?
   * how to collect trace from other Threads?
  * Memory access for instructions
  * DSL implementation for arguments and reading commands
  * Reference type
  * Arguments length: varying amount?
  * implement Constants, loads, stores, math, conversions, comparisons, extended, reserved
  * unsigned byte, other types operated indirectly
  * conversion i2l, etc?
  * implement Control, references
  * implement return, exceptions, monitors, memory access
 * MVP: run a trivial main class
  * Write java Main class, compile, find out commands
  * Program commands
  * Program execution of commands
  * Write trivial class loading and what it takes to execute a static main method

*Javelin deferred tasks*
* need more unit testing for bytecode parser
* test class searching in class path

*Possible offspring projects*
* Continue with implenenting garbage collection
* Continue with JIT
* Viewing step by step bytecode execution interactively
* Decompiler
* UI for viewing bytecode
* Disassembler

*Haskell deferred tasks*
* quickcheck/hunit/tasty/hspec
* checkSpec, htest
* emacs modes
* hlint

*Java 7 -> 8 bytecode changes*
* 4.7.24, MethodParameters attribute (new)
* 4.7.20, RuntimeVisibleTypeAnnotations (new)
* 4.7.21, RuntimeInvisibleTypeAnnotations (new)
* Changes in descriptor and signatures terminology (refactoring)

Very handy posts:
* http://dev.stephendiehl.com/hask/
* http://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad
* http://katychuang.com/cabal-guide/
* http://fp.silk.co