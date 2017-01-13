Javelin
=======
JVM SE8 implementation in Haskell

Usage:
* Disassembler

        stack exec javelin disasm acceptance/TreeMultimapNaturalTest.class

* Disassembler with traced references to constant pool

        stack exec javelin disasmFull acceptance/TreeMultimapNaturalTest.class

* Bytecode statistics

        stack exec javelin stats acceptance/ [output.textile]
 Examples:
 * [Guava opcodes statistics](https://gist.github.com/antonlogvinenko/a9d8f813b4ceb4eebf1ebec598882f2a)
 * [Java 8 standard library (whole rt.jar) statistics](https://gist.github.com/antonlogvinenko/e5461abdd1431c231a6a8e7734c04a05)