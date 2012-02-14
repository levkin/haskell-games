
-- Purpose of this file is to implement interleaving between 
-- infinite lazy streams

> {-# Language ImplicitParams #-}

> import Data.List
> import System.IO
> import Data.ByteString 

> data LazyStream a = EOF | Cons { val :: a , nextSt :: LazyStream a }

-- We need structure to control switching between streams
-- Let's try implementing interleave of N streams . Order of streams is defined by list 
 
 
> interleave :: (?step :: Int) => [LazyStream a] -> LazyStream a

> interleave [] = EOF
> interleave (x:xs) | xs == [] = x
>                   | otherwise = undefined


   Since algorithm is round robin there should beginning and the
   How I implement cyclic list here
   I can keep 'reference' to the end . So after I fetch n entries . Push the remainder to the end , unless it's EOF
   Another option  is to implement sort of double buffer lists , where one list is consumed and other is generated and when consumed is empty , buffers are switched


> _interleave :: (?step :: Int) => (LazyStream a) -> [LazyStream a] -> 
>                -> [LazyStream a]  -> LazyStream a



> -- Corner cases

Here we remove leading EOF
  
> _interleave EOF [] [] = EOF  
> _interleave EOF [] (z : []) = z
> _interleave EOF [] nextBuf@(EOF:ys) = _interleave EOF [] ys
> _interleave EOF [] (y:ys) = _interleave y [] ys
> _interleave EOF (x:xs) _next = _interleave x xs _next 
                                 
More general case                                 
Current buffer is empty -> Swap with next buffer
Swap current and next buffer                                 
                                 
> _interleave stream [] nextBuf@(y:ys) = _interleave stream nextBuf [] 
  
Common case                 

> _interleave stream@(Cons val nextStep) currBuf@(x:xs) nextBuf@(y:ys) = 
>     Cons val newNextStep  where
  
       Skip step entries then add remainder of a stream to next buffer 
       
  
>        (newNextStep,streamContinuation) = _fetch ?step nextStep x       
>        _fetch :: Int -> (LazyStream a) -> (LazyStream a) -> (LazyStream a,LazyStream a)                                            
>        _fetch _num EOF _fromBufStr = (_fromBufStr,EOF)          
>        _fetch _num _currStr _fromBufStr | _num <= 0 = ((Cons (val _currStr) _fromBufStr),(nextSt _currStr))         
>        _fetch _num _currStr _fromBufStr | otherwise = _fetch (_num - 1) (nextSt _currStr _fromBufStr )        

