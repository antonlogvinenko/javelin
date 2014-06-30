module Javelin.Util

where
import Control.Monad.Trans.Maybe

(!?) :: (Integral i) => [a] -> i -> Maybe a
(!?) list index = let indexInt = fromIntegral index in
  if length list <= indexInt
  then Nothing
  else Just $ list !! indexInt

-- These are some missing Haskell functions, need to move somewhere else
toMaybeT :: (Monad m) => Maybe a -> MaybeT m a
toMaybeT = MaybeT . return

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e (Just a) = Right a
maybeToEither e Nothing = Left e

-- Srsly, Haskell, where is my 'replace' function?
replace :: Char -> Char -> String -> String
replace co cr = map (\c -> if c == co then cr else c)


data Apply a b = Apply { x :: a, f :: a -> b }

(->>) :: a -> (a -> b) -> Apply a b
(->>) x f = Apply x f

(.>) :: Apply a b -> (b -> c) -> Apply a c
(.>) (Apply x f) g = Apply x $ g . f

run :: Apply a b -> b
run (Apply x f) = f x
