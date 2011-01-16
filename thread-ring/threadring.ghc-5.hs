-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- Contributed by Jed Brown with improvements by Spencer Janssen and Don Stewart
-- Modified by Alex Mason
-- 
-- 503 threads are created with forkIO, with each thread creating one
--  synchronised mutable variable (MVar) shared with the next thread in the
--  ring. The last thread created returns an MVar to share with the first
--  thread. Each thread reads from the MVar to its left, and writes to the
--  MVar to its right.
-- Each thread then waits on a token to be passed from its neighbour. Tokens
--  are then passed around the threads via the MVar chain N times, and the
--  thread id of the final thread to receive a token is printed.
-- 
-- More information on Haskell concurrency and parallelism
--  [http://www.haskell.org/ghc/dist/current/docs/users_guide/lang-parallel.html].
-- 
-- Compile with:
--      /usr/local/src/ghc-6.10.1/bin/ghc --make -O2 -fglasgow-exts threadring.ghc-4.hs -o threadring.ghc_run
-- 
-- Run as:
--      ./threadring.ghc_run 50000000

import Control.Monad
import Control.Concurrent
import System.Environment
import System.Exit

ring = 503

new res l i = do
  r <- newEmptyMVar
  forkIO (thread res i l r)
  return r

thread :: MVar () -> Int -> MVar Int -> MVar Int -> IO ()
thread res i l r = go
  where go = do
          m <- takeMVar l
          when (m == 1) (print i >> putMVar res ())
          putMVar r $! m - 1
          when (m > 0) go

main = do
  a <- newMVar . read . head =<< getArgs
  ret <- newEmptyMVar
  z <- foldM (new ret) a [2..ring]
  forkIO (thread ret 1 z a)
  takeMVar ret