
-- Tried examples from haskell wiki
-- As far as I understand , idea is as follows .
-- Normal version : f :: a -> b -> c
-- CPS version : f_cps :: a -> b -> ( c -> r) -> r
-- ( c -> r ) -- Continuation , I guess -- What to do with result .
-- After it's application -> Result is applied 
-- Also . Assuming we would like to continue with another function : 
-- g :: c -> c1 -> ( c2 -> r ) -> r . To get something like f . g , we get 
-- f x y $ \z -> g z z2 k . Type of \z -> g z z2 k is (c -> r) that may serve as correct continuation

add_x_y_cps :: Int -> Int -> (Int -> r) -> r
add_x_y_cps x y k =  k (x + y)



square_cps :: Int -> (Int -> r) -> r
square_cps x k  =  k (x * x)



pythagoras_cps :: Int -> Int -> (Int -> r) -> r
pythagoras_cps x y k =
 square_cps x $ \x_squared ->
 square_cps y $ \y_squared ->
 add_x_y_cps x_squared y_squared $ \sum_of_squares ->
 k sum_of_squares


-- My implementation of Cont monad
-- Check monadic laws afterwards


newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
  -- a -> m a

  -- For any function f of type (a->r) , result
  -- will be applying of argument of return to this function
  return x = Cont $ \f -> f x

  -- (>>=) :: m a -> (a -> m b) -> m b
  -- k :: (b->r)
  -- Need to create input to f
  -- runCont m -> (a->r) -> r
  -- ( a -> r ) = \a -> (f a)

  -- 
  -- k :: (b->r) . This is type of continuation after bind
  -- runCont m :: (a -> r) -> r . We need to build function a -> r
  -- What we are doing to get ( a -> r ) .. \a -> <Something that has type r> . 
  -- runCont (f x) k will return r . We build continuation that depends on a , transformation ( a -> Cont r b) 
  m >>= f  = Cont $ \k -> (runCont m)  (\x -> runCont (f x) k) 


square_cps2 :: (Monad m) => Int -> m Int 
square_cps2 x = return $ x * x 

add_x_y_cps2 :: (Monad m) =>  Int -> Int -> m Int
add_x_y_cps2 x y = return $ x + y 


test1 x y = do
  x_sq <- square_cps2 x
  y_sq <- square_cps2 y
  result <- add_x_y_cps2 x_sq y_sq 
  return result

mb :: Int ->  Maybe Int
mb x = if x > 1000 then (Just x) else Nothing

print_res x y = (runCont $ test1 x y) print
checkMaybe x y = (runCont $ test1 x y) mb
  
