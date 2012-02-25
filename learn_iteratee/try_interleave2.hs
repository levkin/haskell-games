{-# Language ImplicitParams #-}
import qualified Data.ListLike as LL

interleave :: (?step :: Int) => [[a]] -> [a]

interleave [] = []
interleave (z:[]) = z

interleave (z:zs) = _interleave2 z zs []
-- Resulting list should be p
-- _interleave2  + take N entries and append it to "NEXT" queue
-- This may probably look like right fold 
-- problem is managing 
_interleave2 :: (?step :: Int) => [a] -> [[a]] -> [[a]] -> [a]
_interleave2 [] [] [] = []
_interleave2 [] [] (n:ns) = _interleave2 n ns []
_interleave2 [] (c:cs) kuku = _interleave2 c cs kuku
_interleave2 currLst [] [] = currLst
_interleave2 currLst [] (n:ns) = _interleave2 currLst (n:ns) []
_interleave2 currLst (c:cs) kuku = let
      p = \x ->  ((take ?step x),(drop ?step x)) 
      (_step,_cont) = p currLst
    in (++)_step $ _interleave2 c cs (kuku ++ [_cont])


--------------------------------------------------
--   Tests
--------------------------------------------------
x1 = [1..10]   
x2 = [20..40]
x3 = [40..50]
x4 = [1..]   
x5 = [20..]
x6 = [40..]
int1 = let ?step = 2 in interleave

test0 = int1 [x1]
test1 = int1 [x1,x2,x3]
test2 = take 20 $ int1 [x4,x5,x6]
