Phase I
* field resolution p361
* method resolution
* preparation
* initialization
* execution of commands
* establish min integration testing
* Run sum of two numbers and println result
* Make printing Runtime readable: what is important in its contents?

Phase II
- all instructions
- exceptions, monitor locks
- resolution: interface methods, etc
- constaints in loading
- access rights in linking
- custom class loaders

*Long term*
* User classpath suport
* Improve class path handling
  * simpler zip condition - check file signature?
  * .class files in path
  * zip, ear, war files in classpath
  * rename JarFile to ZipFile
  * recursibe Zip files
  * common code with statistics?
  * use vector for bytecodes

*notes*
in load function - maybe return tuple with runtime
runtime constant pool must be map
	- not all indices are referenced, see definition of deriveRef - empty results there
SpecifyMeError - replace with detailed, maybe use LinkageError; add messages
VMError and contents: add messages
VMError: use interfaces, less data types? not sure

- why check not already asked by initCL, but then fail?
- how can be loaded multiple times? different init CLs?
- why runtime package? different initCL !=> different defining CL?


*Possible offspring projects*
* Continue with implenenting garbage collection
* Continue with JIT
* Viewing step by step bytecode execution interactively
* Decompiler
* UI for viewing bytecode
* Disassembler


Class loading https://pbs.twimg.com/media/BtcuJK5IcAA7DxK.jpg

Very handy posts:
* http://dev.stephendiehl.com/hask/
* http://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad
* http://katychuang.com/cabal-guide/
* http://fp.silk.co
* http://ejenk.com/blog/why-dependently-typed-programming-will-one-day-rock-your-world.html
* http://www.seas.upenn.edu/~cis194/lectures.html
* https://github.com/grsmv/haskell-bookmarks
