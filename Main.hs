newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ do maybeValue <- runMaybeT x
                        case maybeValue of
                          Nothing    -> return Nothing
                          Just value -> runMaybeT $ f value

newtype State s a = State { runState :: s -> (a,s) }
instance Monad (State s) where
  return x = State $ \s -> (x,s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s
                                      (State g) = f a
                                  in  g newState

--lift = undefined

--lengthCounter len string = if (length string) == len then 1 else 0

type Counter = Integer -> MaybeT (State [String]) Integer

increment :: Counter
increment count = MaybeT $ State $ (\strings -> (Just(count + 1), strings))

decrement :: Counter
decrement count = MaybeT $ State $ (\strings -> (Just(count - 1), strings))

breaking :: Counter
breaking count = MaybeT $ State $ (\strings -> (Nothing, strings))

countAll :: [String] -> (Maybe Integer, [String])
countAll = runState $ runMaybeT $ return 0 >>= increment >>= increment >>= breaking >>= decrement

main = do
  let c = fst $ countAll ["a", "b"] in
    case c of
      Just v -> putStrLn "cake"
      Nothing -> putStrLn "Broken!"
  putStrLn "asd"
