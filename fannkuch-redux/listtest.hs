{-#LANGUAGE BangPatterns #-}
import Data.Int


fan :: Int -> [Int8] -> Int
fan !n (1:xs) = n
fan !n (2:a:xs) = fan (n+1) (a:2:xs)
fan !n (3:a:b:xs) = fan (n+1) (b:a:3:xs)
fan !n (4:a:b:c:xs) = fan (n+1) (c:b:a:4:xs)
fan !n (5:a:b:c:d:xs) = fan (n+1) (d:c:b:a:5:xs)
fan !n (6:a:b:c:d:e:xs) = fan (n+1) (e:d:c:b:a:6:xs)
fan !n (7:a:b:c:d:e:f:xs) = fan (n+1) (f:e:d:c:b:a:7:xs)
fan !n (8:a:b:c:d:e:f:g:xs) = fan (n+1) (g:f:e:d:c:b:a:8:xs)
fan !n (9:a:b:c:d:e:f:g:h:xs) = fan (n+1) (h:g:f:e:d:c:b:a:9:xs)
fan !n (10:a:b:c:d:e:f:g:h:i:xs) = fan (n+1) (i:h:g:f:e:d:c:b:a:10:xs)
fan !n (11:a:b:c:d:e:f:g:h:i:j:xs) = fan (n+1) (j:i:h:g:f:e:d:c:b:a:11:xs)
fan !n (12:a:b:c:d:e:f:g:h:i:j:k:xs) = fan (n+1) (k:j:i:h:g:f:e:d:c:b:a:12:xs)
