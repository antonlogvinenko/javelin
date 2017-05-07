Javelin
=======
JVM SE8 implementation in Haskell

Usage:
* Decompiler

        stack exec javelin disasm acceptance/TreeMultimapNaturalTest.class
 [See result here](https://gist.github.com/antonlogvinenko/9a6dcc4dbabe0acef90df3a7f9fd7d0b)

* Decompiler with traced references to constant pool

        stack exec javelin disasmFull acceptance/TreeMultimapNaturalTest.class
 [See result here](https://gist.github.com/antonlogvinenko/cdc157a251efe965b9af2244ba41fcf6)

* Bytecode statistics

        stack exec javelin stats acceptance/ [output.textile]
 Examples: [Guava opcodes statistics](https://gist.github.com/antonlogvinenko/a9d8f813b4ceb4eebf1ebec598882f2a), [Java 8 standard library (rt.jar) statistics](https://gist.github.com/antonlogvinenko/e5461abdd1431c231a6a8e7734c04a05)

* Parsing classpath, traversing it and loading class bytes

        stack exec javelin classpath "./sample-classpath/" "com/google/common/collect/Lists"

* Loading class with all dependencies

        exec javelin classpath "./sample-classpath/" "com/google/common/collect/Lists"
	