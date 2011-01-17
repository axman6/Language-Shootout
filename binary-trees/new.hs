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
import Data.Array.Base
import Data.Array.ST
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
    let maxN        = max (minN + 2) n
        stretchN    = maxN + 1
        c           = check (make 0 stretchN)
        -- allocate a long lived tree
        long       = make 0 maxN
        -- allocate, walk, and deallocate many bottom-up binary trees
        vs          = depth minN maxN `using` parList rdeepseq
    -- ensure long is evaluates first
    c `par`long `pseq` io "stretch tree" stretchN c
    mapM_ (\((m,d,i)) -> io (show m ++ "\t trees") d i) vs
    -- confirm the the long-lived binary tree still exists
    io "long lived tree" maxN (check long)

-- generate many trees
depth :: Int -> Int -> [(Int,Int,Int)]
depth d' m = go d' where
    go d | d <= m    = (2*n,d,sumT d n 0) : depth (d+2) m
         | otherwise = []
         where !n = 1 `shiftL` (m - d + minN)

-- allocate and check lots of trees
sumT :: Int -> Int -> Int -> Int
sumT d i' t' = go i' t' where
    go 0 t = t
    go i t =  a `par` b `pseq` go (i-1) $! (t + a + b)
      where a = check (make i    d)
            b = check (make (-i) d)

check :: Tree -> Int
check arr = pgo 1 where
    pgo i | i < 4 = l `par` r `pseq` indx i + l - r
          | otherwise = go i where
              l = pgo i'
              r = pgo (i'+1)
              !i' = i+i

    go !i | i >= end = 0
    go i = let !i' = i+i
               l = go $ i'   -- left child
               r = go $ i'+1 -- right child
         in indx i + l - r
    !end = snd . bounds $ arr
    indx i = unsafeAt arr (i-1)

make :: Int -> Int -> Tree
make i d = runSTUArray $ do
    let !end = 2^(d+1)
    arr <- newArray (1,end) 0
    writeA arr 1 i
    forM_ [2..end] $ \ix -> do
        p <- readA arr $ ix `shiftR` 1 -- parent
        writeA arr  ix $! p + p + (if even ix then -1 else 0)
    return arr where
    readA arr ix = unsafeRead arr (ix-1)
    writeA arr ix v = unsafeWrite arr (ix-1) v
