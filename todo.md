*Current*
* commands execution
 * invokestatic
 * nextCommand
 * VM exit

*Next*
 * addition
 * println

*Fixes*
* User classpath suport
* Improve class path handling
  * simpler zip condition - check file signature?
  * .class files in path
  * zip, ear, war files in classpath
  * rename JarFile to ZipFile
  * recursibe Zip files
  * common code with statistics?
  * use vector for bytecodes
* SpecifyMeError - replace with detailed, maybe use LinkageError; add messages
* VMError and contents: add messages
* VMError: use interfaces, less data types? not sure
* preparation: keep prepared classes as set
*  error states
* io monad

*Missing, planned:*
* init
* some LLI parts

*Not planned:*
 * loading constraints
 * visibility
 * verification

* Whole JVM *
 * Schema
   * commands
     * static commands require init/linking (linking = verif+prep)
     * non-static commands require resolving
   * resolving can be separate from init/linking
   * loading class/interface require resolving its parents
   * resolving part of a class/interface requires resolving the class/interfase itself
 * loading constraints/visibility
 * errors while loading, reusing already existing status
 * referencing classes by name/loading/initing class loader

*Open questions*
* why check not already asked by initCL, but then fail?
* how can be loaded multiple times? different init CLs?
* why runtime package? different initCL !=> different defining CL?

*Possible offspring projects*
* Continue with implenenting garbage collection
* Continue with JIT
* Viewing step by step bytecode execution interactively
* Decompiler
* UI for viewing bytecode
* Disassembler

*Useful:*
* http://dev.stephendiehl.com/hask/
* http://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad
* http://katychuang.com/cabal-guide/
* http://fp.silk.co
* http://ejenk.com/blog/why-dependently-typed-programming-will-one-day-rock-your-world.html
* http://www.seas.upenn.edu/~cis194/lectures.html
* https://github.com/grsmv/haskell-bookmarks

Class loading https://pbs.twimg.com/media/BtcuJK5IcAA7DxK.jpg
