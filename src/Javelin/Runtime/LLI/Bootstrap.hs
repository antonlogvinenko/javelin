module Javelin.Runtime.LLI.Bootstrap
where 

import Javelin.Runtime.Thread (Runtime, newRuntime, Trace, startWithMain)
  
bootstrap :: String -> Runtime -> Runtime
bootstrap className runtime = runtime


runJVM :: String -> [String] -> Trace
runJVM className mainArgs = let runtime = bootstrap className newRuntime
                            in startWithMain runtime className mainArgs True
