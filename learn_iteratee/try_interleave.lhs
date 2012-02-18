
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
> interleave (x:[]) = x
> interleave (x:xs) = undefined


   Since algorithm is round robin there should beginning and the
   How I implement cyclic list here
   I can keep 'reference' to the end . So after I fetch n entries . Push the remainder to the end , unless it's EOF
   Another option  is to implement sort of double buffer lists , where one list is consumed and other is generated and when consumed is empty , buffers are switched


> _interleave :: (?step :: Int) => (LazyStream a) -> [LazyStream a] 
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
Swap current and next buffer .
Continuation stream is appended to result of fetch
Remainder of original stream is inserted to nextBuf

                                 
> _interleave stream [] nextBuf@(y:ys) = _interleave stream nextBuf [] 
  
Common case                 

> _interleave stream@(Cons val nextStep) currBuf@(x:xs) nextBuf@(y:ys) = 
>     result  where
  
Convert n entries and then append continuation 

>   _fetch :: Int -> (LazyStream a) -> (LazyStream a -> (LazyStream a {- New stream -}, LazyStream a {- Original continuation-}))  

Default continuation implements processing of current stream entry :
Given entry and continuation , it appends continuation and returns original continuation

>   _defaultCont :: (LazyStream a) {-Entry-} -> (LazyStream a) {-Continuation-} ->  (LazyStream a {-Result-} , LazyStream a {- Original continuation -})
>   _defaultCont EOF cont = (cont,EOF)  
>   _defaultCont pos@(Cons _val _next) cont = ((Cons _val cont),_next)

If current stream is empty , continue to 'continuation'

Skip : Operate on current position and next as arguments
Also , at the end the last 'next' position will be used for continuation + adding to a buffer
Passing next as an explitcit argument allowed using skip as tail recursion and allowed accessing result of 
last recursion step

>   _skip :: Int -> (LazyStream a {-current pos-},LazyStream a {-next pos-}) -> (LazyStream a,LazyStream a)    
>   _skip _ (EOF,_) = (EOF,EOF)
>   _skip n (a@(Cons _val _),b) | n <= 0 = (a,b)
>             | otherwise = _skip (n-1) (b, nextSt b)


>   _fetch num EOF cont = _defaultCont EOF cont 
>   _fetch num pos cont | num <= 0 = _defaultCont pos cont
>   _fetch num pos@(Cons _val _next) cont | num > 0 = (_result,_origCont) where
>     (skipped,_origCont)  = _skip num (pos,_next)
>     _result = case skipped of 
>       Cons jj _ ->  Cons jj cont
>       EOF       ->  cont

Here we are implementing general case of _interleave

>   (afterFetch,origCont) = _fetch ?step stream x 
>   result = _interleave afterFetch xs (nextBuf ++ [origCont]) 

