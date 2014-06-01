module Javelin.Runtime.LLI.Bootstrap
where 

import Javelin.Runtime.Thread (Runtime, newRuntime, startWithMain)
  
bootstrap :: String -> Runtime -> Runtime
bootstrap className runtime = runtime


runJVM :: String -> [String] -> ()
runJVM className mainArgs = let runtime = bootstrap className newRuntime
                            in startWithMain runtime className mainArgs
