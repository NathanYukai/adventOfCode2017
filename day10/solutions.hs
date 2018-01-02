
-- fst :: cur position, 
-- snd :: skip size
type State = (Int, Int)

twist :: ([Int], State) -> Int -> ([Int], State)
twist (cur, stt) len  
  | wrap = wrapTwist cur stt len
    | otherwise = noWrapTwitst cur stt len
    where wrap = fst stt + len > length cur
          flipFrontLen = fst stt + len - length cur


noWrapTwitst :: [Int] -> State -> Int -> ([Int], State)
noWrapTwitst cur stt len = ((front ++ (reverse middle) ++ rest), increaseStt stt len (length cur)) 
    where (front, tmp_tail) = splitAt (fst stt) cur
          (middle, rest) = splitAt len tmp_tail 

increaseStt :: State -> Int -> Int -> State
increaseStt (pos, sk) len maxL = (((pos + len + sk) `mod` maxL), (sk+1) `mod` maxL)

wrapTwist ::  [Int] -> State -> Int -> ([Int], State)
wrapTwist cur stt len = (putBack (reversed ++ middle) ((length cur - fst stt) `mod` (length cur)), increaseStt stt len (length cur))
    where wrapLength = fst stt + len - length cur
          (front, tmp_tail) = splitAt wrapLength cur
          (middle, rest) = splitAt (fst stt - wrapLength) tmp_tail
          reversed = reverse (rest ++ front)
         
putBack :: [Int] -> Int -> [Int]
putBack list pos = s ++ f
    where (f,s) = splitAt pos list

answer :: [Int] -> State -> [Int] -> [Int]
answer origin stt lens = fst $ foldl twist (origin, stt) lens
