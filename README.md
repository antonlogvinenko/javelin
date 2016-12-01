Javelin
=======
JVM 8 spec implementation in Haskell.

Modes
* **Disassembler**
  stack exec javelin c acceptance/TreeMultimapNaturalTest.class

*Long term*
* Bytecode parser [**Done**]
* Runtime data structures [**Done**]
* Loading
 * Symtable, 2 cases [**In progress**]
 * Deriving class [**In progress**]
 * Top level class loading [**In progress**]
 * Test these parts of loading
 * Change type of class loading status type
* Linking, initializing: light version [**In progress**]
* Executing instructions
* All features skipped
 * verification
 * exceptions
 * synchronization
 * invokedynamic
 * multiple threads (use haskell threads and transactions for shared memory?)
 * user derined class loaders (only boostrap)

1. Bytecode printing:
 - Commands
   - use vector
   - Cleanup:
     - pretty print ref to constant pool
     - inside reference following formatting
     - common formatting
 - move printing to another file?
 - Running disassembler + doc
 - Write test on many files
 - Write application: parser analyzer, cmd stats

2. Write loading part
 - print all runtime structures

3. Simple executions of commands


1. Loading
 - printing classes
 - printing current runtime (with derived constant pool)
 - fix runtime structures
 - loading array class
 - update scheme of LLI on image (loading -> resolve)
 - check derive
 - check bytes loading
 - errors during class loading

2. Resolving
3. Linking, init
4. Altogether
 
- represent states in Runtime (lli, resolved per class items)
- represent errors
- go through loading class from FS via bootstrap CL
- VM start, init, running bytecode



*Short term*
* Resolving
 * class interface
   * check access
 * for each type of 'resolvables':
   * resolve-methods arguments?
   * implement
   * how and where to store/group resolve results? result is Maybe VMError
* Initializing
 * ???
* Linking + Loaading + Initializing
* Execution of a simple Main class


* ad hoc loading of System, System.out to println data, all required instrucitons (milestone)
* init thread stack to run Main { public static void main(Sting...) }

*Instructions deferred tasks*
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

Class loading https://pbs.twimg.com/media/BtcuJK5IcAA7DxK.jpg

Very handy posts:
* http://dev.stephendiehl.com/hask/
* http://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad
* http://katychuang.com/cabal-guide/
* http://fp.silk.co
* http://ejenk.com/blog/why-dependently-typed-programming-will-one-day-rock-your-world.html
* http://www.seas.upenn.edu/~cis194/lectures.html
* https://github.com/grsmv/haskell-bookmarks