module Javelin.Runtime.LLI.Bootstrap
where 

import Javelin.Runtime.Thread (Thread, newThread, Trace(..), execute)

-- 1. set frame for main method, set pc, set String args
-- 2. load runtime data structures for Main class
bootstrap :: Thread -> String -> [String] -> Thread
bootstrap thread className mainArgs = thread

runJVM :: String -> [String] -> Trace
runJVM className mainArgs = let thread = bootstrap newThread className mainArgs
                            in execute thread True Trace
