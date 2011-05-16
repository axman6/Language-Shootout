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
module Main where

import System
import Data.Word
import Control.Arrow

import Data.List

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C (pack)
import qualified Data.ByteString as S
import Data.ByteString.Internal

main = do
    n <- getArgs >>= readIO . head
    writeFasta  "ONE"   "Homo sapiens alu"       (n*2) (L.cycle alu)
    g <- unfold "TWO"   "IUB ambiguity codes"    (n*3) (mkCacher $ cdfize iubs) 42
    unfold      "THREE" "Homo sapiens frequency" (n*5) (mkCacher $ cdfize homs) g

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
unfold :: [Char] -> [Char] -> Int -> Cacher -> Int -> IO Int
unfold lab ttl n probs gen =
    putStrLn (">" ++ lab ++ " " ++ ttl) >> unroll probs gen n

unroll :: Cacher -> Int -> Int -> IO Int
unroll probs = loop
  where
    loop r 0  = return r
    loop !r i =
        case S.unfoldrN m (Just . look probs) r of
            (!s, Just r') -> do
                S.putStrLn s
                loop r' (i-m)
      where m = min i 60

look cr k = (choose cr newran, newseed)
  where !newseed = {-# SCC "newseed" #-} (k * ia + ic) `rem` im
        !newran  = {-# SCC "newran" #-}  fromIntegral newseed / imd

-- Chunk the list into parts, still can represent any list of the
-- symbol/probability pairs.
data PPair = PPair !Word8 !Float
-- data Cacher = Cacher !PPair !PPair !PPair !PPair Cacher
--             | CacheList ![PPair]

data Cacher = Branch !Float !Word8 !Cacher !Cacher
            | Leaf !Float !Word8

getProb :: Cacher -> Float
getProb (Leaf p _) = p
getProb (Branch p _ _ _) = p 

getWord :: Cacher -> Word8
getWord (Leaf _ w) = w
getWord (Branch _ w _ _) = w

mkCacher :: [Cacher] -> Cacher
mkCacher [x] = x
mkCacher xs = mkCacher (joiner xs)
    where joiner (x:y:zs) = Branch (getProb x) (getWord x) x y : joiner zs
          joiner zs = zs

printCacher :: Cacher -> String
printCacher (Leaf p w) = show p ++ " -> " ++ show w
printCacher (Branch p w l r) =
        show p ++ " -> " ++ show w ++ "\n" ++ (indent (printCacher l) ++ indent (printCacher r))
    where indent = unlines . map ("|   "++) . lines

-- mkCacher (p1:p2:p3:p4:ds) = Cacher p1 p2 p3 p4 (mkCacher ds)
-- mkCacher ds = CacheList ds

cdfize :: [(Word8,Float)] -> [Cacher]
cdfize ds = init cdf' ++ [Leaf 1.0 s]
    where
      Leaf _ s = last cdf'
      cdf' = (snd . mapAccumL go 0) ds
      go c (sym, prob) = (prob', Leaf prob' sym) where !prob' = c+prob

-- We still query the list in order, but we don't have to continually
-- ``uncons'' everything, we can do the first 4 at a time.
choose :: Cacher -> Float -> Word8
choose ch p = choose' ch
    where
        choose' (Branch q w l r) =
            case compare p q of
                LT -> choose' l
                GT -> choose'' r w
                EQ -> w
        choose' (Leaf _ w) = w
        choose'' (Branch q w l r) w' =
            case compare p q of
                LT -> w'
                GT -> choose'' l w
                EQ -> w
        choose'' (Leaf q w) w' = if p <= q then w else w'
        
        
-- choose xs p = choose' xs
--     where
--         choose' :: Cacher -> Word8
--         choose' (Cacher (PPair s1 c1) (PPair s2 c2) (PPair s3 c3) (PPair s4 c4) ds)
--             | p <= c1 = s1
--             | p <= c2 = s2
--             | p <= c3 = s3
--             | p <= c4 = s4
--             | otherwise = choose' ds
--         choose' (CacheList ds) = chooseCdf ds
-- 
--         chooseCdf :: [PPair] -> Word8
--         chooseCdf ((PPair b f):xs) = if p < f then b else chooseCdf xs

------------------------------------------------------------------------
--
-- only demand as much of the infinite sequence as we require

writeFasta :: [Char] -> [Char] -> Int -> L.ByteString -> IO ()
writeFasta label title n s = do
     putStrLn $ ">" ++ label ++ " " ++ title
     let (t:ts) = L.toChunks s
     go ts t n
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