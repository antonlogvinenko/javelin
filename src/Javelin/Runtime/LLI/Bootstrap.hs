module Javelin.Runtime.LLI.Bootstrap
where 

import Javelin.Runtime.Thread (Thread, newThread, Trace(..), execute)

-- 1. set frame for main method, set pc, set String args
-- 2. load runtime data structures for Main class
bootstrap :: String -> Thread -> String -> [String] -> Thread
bootstrap classPath thread className mainArgs = thread

runJVM :: String -> String -> [String] -> Trace
runJVM classPath className args = let thread = bootstrap classPath newThread className args
                                  in execute thread True Trace
