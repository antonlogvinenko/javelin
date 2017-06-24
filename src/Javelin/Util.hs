module Javelin.Util

where

(!?) :: (Integral i) => [a] -> i -> Maybe a
(!?) list index = let indexInt = fromIntegral index in
  if length list <= indexInt
  then Nothing
  else Just $ list !! indexInt

(#) :: (Integral i) => [a] -> i -> a
(#) list index =
  let indexInt = fromIntegral index - 1
  in list !! indexInt

at :: Integral b => [a] -> b -> a
at cc i = cc !! ((fromIntegral i) - 1)
