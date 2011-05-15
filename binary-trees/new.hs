{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
--
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
--
-- Contributed by Don Stewart
-- Modified by Stephen Blackheath to parallelize (a very tiny tweak)
--

import System
import Data.Bits
import Text.Printf
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Array.Base
import Data.Array.ST
import Data.List
import Control.Monad
import GHC.Conc
--
-- an artificially strict tree.
--
-- normally you would ensure the branches are lazy, but this benchmark
-- requires strict allocation.
--
type Tree = UArray Int Int

minN :: Int
minN = 4

io :: String -> Int -> Int -> IO ()
io s n t = printf "%s of depth %d\t check: %d\n" s n t

main :: IO ()
main = do
    n <- getArgs >>= readIO . head
    -- let n = 20
    let maxN        = max (minN + 2) n
        stretchN    = maxN + 1
        c           = check (make 0 stretchN)
        -- allocate a long lived tree
        long       = make 0 maxN
        -- allocate, walk, and deallocate many bottom-up binary trees
        vs          = depth minN maxN `using` parList rdeepseq
    -- ensure long is evaluates first
    c `par` long `par` io "stretch tree" stretchN c
    mapM_ (\(P3 m d i) -> io (show m ++ "\t trees") d i) vs
    -- confirm the the long-lived binary tree still exists
    io "long lived tree" maxN (check long)

data P3 = P3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
instance NFData P3 where
    rnf (P3 x y z) = ()

-- generate many trees
depth :: Int -> Int -> [P3]
depth d' m = go d' where
    go d | d <= m    = P3 (2*n) d (sumT d n 0) : depth (d+2) m
         | otherwise = []
         where !n = 1 `shiftL` (m - d + minN)

-- allocate and check lots of trees
sumT :: Int -> Int -> Int -> Int
sumT d i' t' = go i' [t'] where
    go 0 t = foldl' (+) 0 t
    go i t =  a `par` b `par` go (i-1) (a:b:t)
      where a = check (make i    d)
            b = check (make (-i) d)

check :: Tree -> Int
check !arr = pgo 1 where
    pgo i | i < 4 = r `par` l `pseq` indx i + l - r
          | otherwise = go i  where
              l = {-# SCC "pgoleft" #-}  pgo i'
              r = {-# SCC "pgoright" #-} pgo (i'+1)
              !i' = i+i
    go i | i < end =
        case go (i+i) of
            l -> case go (i+i+1) of
                r -> case indx i of
                    x -> l + x - r
    go _ = 0
    -- go !i x | i < end = let !i' = {-# SCC "i" #-} i + i in go i' (indx i + (negate $ go (i+i+1) x))
    !end = snd . bounds $ arr
    indx i = unsafeAt arr (i-1)

make :: Int -> Int -> Tree
make i d = runSTUArray $ do
    let !end = {-# SCC "2pow" #-} 2^(d+1)
    arr <- newArray (1,end) 0
    writeA arr 1 i
    forM_ [2,4..end] $ \ix -> do
        p <- readA arr $ ix `shiftR` 1 -- parent
        let !p' = p + p
        writeA arr ix        $! pred p'
        writeA arr (succ ix) $! p'
    return arr where
    readA arr ix = unsafeRead arr (pred ix)
    writeA arr ix v = unsafeWrite arr (pred ix) v
