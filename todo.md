* Current *
* fix isInterface and alike annotations
  parse accesses for class/field/method
* checks that it was already resolved?

* Fixes *
* User classpath suport
* Improve class path handling
  * simpler zip condition - check file signature?
  * .class files in path
  * zip, ear, war files in classpath
  * rename JarFile to ZipFile
  * recursibe Zip files
  * common code with statistics?
  * use vector for bytecodes

* Notes *
was processed (loaded/resolved)errors - repeat?
add access check
add restrictions check

SpecifyMeError - replace with detailed, maybe use LinkageError; add messages
VMError and contents: add messages
VMError: use interfaces, less data types? not sure

- why check not already asked by initCL, but then fail?
- how can be loaded multiple times? different init CLs?
- why runtime package? different initCL !=> different defining CL?


* Possible offspring projects *
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
