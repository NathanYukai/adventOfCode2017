import Numeric
import Data.Char 

getNextValue :: Int -> Int -> Int
getNextValue cur mul = cur*mul `mod` 2147483647

compareLowest16 :: Int -> Int -> Bool
compareLowest16 a b = complow16 binA binB
    where binA = showIntAtBase 2 intToDigit a ""
          binB = showIntAtBase 2 intToDigit b ""


complow16 :: String -> String -> Bool
complow16 c1 c2 = getComp c1 == getComp c2
    where getComp = \a -> fst $ splitAt 16 $ reverse a


getSequence :: Int -> Int -> Int -> Int -> [Int]
getSequence start mult len filt = tail $ take (len+1) $ filter (\a -> a `mod` filt ==0)$ iterate (getNextValue mult) start

findTotalMatch :: Int -> Int -> Int -> Int -> Int -> Int
findTotalMatch aStart aMult bStart bMult len = length $ filter id allCompare
    where sequenceA = getSequence aStart aMult len 4
          sequenceB = getSequence bStart bMult len 8
          allCompare = zipWith compareLowest16 sequenceA sequenceB


