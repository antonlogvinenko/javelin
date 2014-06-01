module Javelin.Runtime.LLI.Bootstrap
where 

import Javelin.Runtime.Thread (Thread, newThread, Trace(..), execute)

-- 1. set frame for main method, set pc, set String args
-- 2. load runtime data structures for Main class
bootstrap :: String -> Thread -> Thread
bootstrap className thread = thread

runJVM :: String -> [String] -> Trace
runJVM className mainArgs = let thread = bootstrap className newThread
                            in execute thread True Trace
