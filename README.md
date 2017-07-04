Javelin
=======
JVM SE8 implementation in Haskell

Usage:
* Disassemler

        stack exec javelin disasm acceptance/TreeMultimapNaturalTest.class
 [See result here](https://gist.github.com/antonlogvinenko/9a6dcc4dbabe0acef90df3a7f9fd7d0b)

* Disassembler with traced references to constant pool

        stack exec javelin disasmFull acceptance/TreeMultimapNaturalTest.class
 [See result here](https://gist.github.com/antonlogvinenko/cdc157a251efe965b9af2244ba41fcf6)

* Bytecode statistics

        stack exec javelin stats acceptance/ [output.textile]
 Examples: [Guava opcodes statistics](https://gist.github.com/antonlogvinenko/a9d8f813b4ceb4eebf1ebec598882f2a), [Java 8 standard library (rt.jar) statistics](https://gist.github.com/antonlogvinenko/e5461abdd1431c231a6a8e7734c04a05)

* Parsing classpath, traversing it and loading class bytes

        stack exec javelin classpath "./sample-classpath/" "com/google/common/collect/Lists"

* Loading class with all dependencies

        stack exec javelin loadClass "./sample-classpath/" "com/google/common/collect/Lists"


Roadmap:
* Phase I, Reconnaissance. Provide the most basic implementation of every level (loading, resolution, init, runtime structures, command execution, etc). These implementations may skip some essential functionality, but will make it possible to execute JVM bytecode. Implementations don't contradict spec, but do not reflect all spec requirements.
  * field resolution
  * method resolution
  * preparation
  * initialization
  * execution of commands
  * establish min integration testing
  * use case: sum of two numbers and println result
  * printing Runtime readable: most important in its contents
* Phase II. Implement stuff that was left out in phase I, but still skip parts of spec that are not of any interest to the author (for now)
  * all instructions
  * exceptions, monitor locks
  * resolution: interface methods, etc
  * constraints in loading
  * access rights in linking
  * custom class loaders


* Phase III, either implement skipped JVM features, or make some kind of product out of this codebase. I have some ideas.