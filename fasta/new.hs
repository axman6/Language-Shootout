{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
--
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
--
-- Contributed by Don Stewart
-- A lazy bytestring solution.
-- Unnecessary strictness annotations removed by Sterling Clover 2/08
--
-- Adding cached version of the list, where the first 4 entries are
-- lifted into the data constructor by Scott West 03/10
--
-- Add:
-- -optc-mfpmath=sse -optc-msse2
--
module Main (main) where

import System
import Data.Word
import Control.Arrow ((***))

import Data.List

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C (pack)
import qualified Data.ByteString as S
import Data.ByteString.Internal

import Data.Array.Unboxed (UArray, array)
import Data.Array.Base (unsafeAt)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar

main = do
    n <- getArgs >>= readIO . head
    output <- newChan :: IO (Chan S.ByteString)
    wait <- newEmptyMVar :: IO (MVar ())
    forkIO $ do
        g <- unfold output "TWO"   "IUB ambiguity codes"    (n*3) (mkCacher $ cdfize iubs) 42
        unfold      output "THREE" "Homo sapiens frequency" (n*5) (mkCacher $ cdfize homs) g
        putMVar wait ()
    writeFasta  "ONE"   "Homo sapiens alu"       (n*2) (L.cycle alu)
    forkIO (printChan output)
    takeMVar wait
    where printChan mv = readChan mv >>= S.putStrLn >> printChan mv

------------------------------------------------------------------------
im, ia, ic :: Int
im  = 139968
ia  = 3877
ic  = 29573
imd :: Float
imd = 139968
------------------------------------------------------------------------

--
-- lazily unfold the randomised dna sequences
--
unfold :: Chan S.ByteString -> [Char] -> [Char] -> Int -> Cacher -> Int -> IO Int
unfold out lab ttl n probs gen =
    putStrLn (">" ++ lab ++ " " ++ ttl) >> unroll out probs gen n

unroll :: Chan S.ByteString -> Cacher -> Int -> Int -> IO Int
unroll out probs = loop where
    loop r 0  = return r
    loop !r i = case S.unfoldrN m (Just . look) r of
                    (!s, Just r') -> writeChan out s >> loop r' (i-m)
      where m      = min i 60
            look k = (choose probs newran, newseed) where
                !newseed = (k * ia + ic) `rem` im
                !newran  = fromIntegral newseed / imd

data PPair = PPair !Word8 !Float
data Cacher = C !(UArray Int Float) !(UArray Int Word8)

mkCacher :: [PPair] -> Cacher
mkCacher xs =
    C (array (0,len) (zip [(0::Int)..] (map (\(PPair _ f) -> f) xs)))
      (array (0,len) (zip [(0::Int)..] (map (\(PPair w _) -> w) xs)))
    where len = length xs


cdfize :: [(Word8,Float)] -> [PPair]
cdfize ds = init cdf' ++ [PPair s 1.0] where
    PPair s _ = last cdf'
    cdf' = (map (uncurry PPair) . snd . mapAccumL go 0) ds
    go c (sym, prob) = (prob', (sym, prob')) where !prob' = c+prob

choose :: Cacher -> Float -> Word8
choose (C freqs vals) p = unsafeAt vals (finder 0)
    where finder n | p <= unsafeAt freqs n = n | otherwise = finder (n+1)

------------------------------------------------------------------------
--
-- only demand as much of the infinite sequence as we require

writeFasta :: [Char] -> [Char] -> Int -> L.ByteString -> IO ()
writeFasta label title width lbs = do
     putStrLn $ ">" ++ label ++ " " ++ title
     let (t:ts) = L.toChunks lbs
     go ts t width
  where
     go ss s n
        | l60 && n60 = S.putStrLn l               >> go ss        r (n-60)
        |        n60 = S.putStr s >> S.putStrLn a >> go (tail ss) b (n-60)
        | n <= ln    = S.putStrLn (S.take n s)
        | otherwise  = S.putStr s >> S.putStrLn (S.take (n-ln) (head ss))
        where
            ln   = S.length s
            l60  = ln >= 60
            n60  = n  >= 60
            (l,r) = S.splitAt 60 s
            (a,b) = S.splitAt (60-ln) (head ss)

------------------------------------------------------------------------


alu = C.pack
        "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
        \GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
        \CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
        \ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
        \GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
        \AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
        \AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iubs, homs :: [(Word8, Float)]
iubs = map (c2w *** id)
        [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
        ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
        ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homs = map (c2w *** id)
        [('a',0.3029549426680),('c',0.1979883004921)
        ,('g',0.1975473066391),('t',0.3015094502008)]