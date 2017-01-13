Javelin
=======
JVM SE8 implementation in Haskell

Usage:
* Disassembler

        stack exec javelin disasm acceptance/TreeMultimapNaturalTest.class
 <a href="gist.github.com/antonlogvinenko/9a6dcc4dbabe0acef90df3a7f9fd7d0b" target="_blank">See result here</a>

* Disassembler with traced references to constant pool

        stack exec javelin disasmFull acceptance/TreeMultimapNaturalTest.class
  <a href="gist.github.com/antonlogvinenko/cdc157a251efe965b9af2244ba41fcf6" target="_blank">See result here</a>

* Bytecode statistics

        stack exec javelin stats acceptance/ [output.textile]
 Examples:
 * <a href="gist.github.com/antonlogvinenko/a9d8f813b4ceb4eebf1ebec598882f2a" target="_blank">Guava opcodes statistics</a>
 * <a href="gist.github.com/antonlogvinenko/e5461abdd1431c231a6a8e7734c04a05" target="_blank">Java 8 standard library (rt.jar) statistics</a>