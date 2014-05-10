module Main
where

import qualified Javelin.Main as M

main :: IO ()
main = do
  putStrLn "Running from outside of the box"
  M.main
