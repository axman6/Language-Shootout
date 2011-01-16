{-# LANGUAGE BangPatterns#-}
{-# OPTIONS -funbox-strict-fields #-}
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
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Parallel
import Control.Parallel.Strategies
import Foreign.Marshal.Alloc (mallocBytes,free)

--
-- an artificially strict tree.
--
-- normally you would ensure the branches are lazy, but this benchmark
-- requires strict allocation.
--
data Tree = Nil | Node !Int !Tree !Tree

minN = 4

io :: String -> Int -> Int -> String
io s n t = printf "%s of depth %d\t check: %d\n" s n t
io' :: String -> Int -> Int -> IO ()
io' s n t = printf "%s of depth %d\t check: %d\n" s n t

main = do
    n <- getArgs >>= readIO . head
    -- spacer <- mallocBytes 524288000 -- 500MB
    -- free spacer
    let maxN     = max (minN + 2) n
        stretchN = maxN + 1

    -- stretch memory tree
    let c = check (make 0 stretchN)
    io' "stretch tree" stretchN c

    -- allocate a long lived tree
    let !long    = make 0 maxN

    -- allocate, walk, and deallocate many bottom-up binary trees
    let vs = parMap rnf id $ depth minN maxN
    -- let vs = depth minN maxN
    
    res <- parallel $ map (\((m,d,i)) -> return $! io (show m ++ "\t trees") d i) vs
    mapM_ putStr res

    -- confirm the the long-lived binary tree still exists
    io'  "long lived tree" maxN (check long)

parallel :: [IO a] -> IO [a]
parallel actions = do
    vars <- forM actions $ \action -> do
        var <- newEmptyMVar
        forkIO $ do
            answer <- action
            return $! answer
            putMVar var answer
        return var
    forM vars takeMVar


-- generate many trees
depth :: Int -> Int -> [(Int,Int,Int)]
depth d m
    | d <= m    = (2*n,d,sumT d n 0) : depth (d+2) m
    | otherwise = []
  where n = 1 `shiftL` (m - d + minN)

-- allocate and check lots of trees
sumT :: Int -> Int -> Int -> Int
sumT !d 0 !t = t
sumT !d !i !t = sumT d (i-1) (t + a + b)
  where a = check (make i    d)
        b = check (make (-i) d)

-- traverse the tree, counting up the nodes
check :: Tree -> Int
check Nil          = 0
check (Node i l r) = i + l' - r'
    where !l' = check l
          !r' = check r

-- build a tree
make :: Int -> Int -> Tree
make !i 0 = Node i Nil Nil
make !i !d = Node i (make (i2-1) d2) (make i2 d2)
  where !i2 = 2*i; !d2 = d-1