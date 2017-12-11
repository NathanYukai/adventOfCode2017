import System.Environment
import Data.List.Split

main = do 
    args <- getArgs
    fileStr <- readFile $ head args
    let 
        steps = splitOn "," $ init fileStr 
        result = foldr (\s l->moveStep l s) (0,0) $ reverse steps
    putStrLn $ show steps
    putStrLn $ show result
    putStrLn $ show $ cubicDistance (axialToCube result) (0,0,0)

-- Axial corordinate
type Loc = (Int,Int)
-- Cubic 
type Cubic = (Int,Int,Int)

moveStep :: Loc -> String -> Loc
moveStep (x,y) "n" = (x+1,y-1)
moveStep (x,y) "ne" = (x+1,y)
moveStep (x,y) "se" = (x,y+1)
moveStep (x,y) "s" = (x-1,y+1)
moveStep (x,y) "sw" = (x-1,y)
moveStep (x,y) "nw" = (x,y-1)


axialToCube :: Loc -> Cubic
axialToCube (q,r) = (q,r,(-q-r))

cubicDistance :: Cubic -> Cubic -> Int
cubicDistance (a,b,c) (x,y,z) = maximum[abs(a-x), abs(b-y), abs(c-z)]



