import System.Environment
import Data.List.Split

main = do 
    args <- getArgs
    fileStr <- readFile $ head args
    let 
        steps = splitOn "," $ init fileStr 
        result = getDistance steps
        allDist = fmap getDistance (allSubSteps steps)
    putStrLn $ show result
    putStrLn $ show allDist
    putStrLn $ show $ maximum allDist

getDistance :: [String] -> Int
getDistance steps = cubicDistance (axialToCube result) (0,0,0)
    where result = foldr (\s l->moveStep l s) (0,0) $ reverse steps

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


allSubSteps :: [a] -> [[a]]
allSubSteps l = fmap (\s -> fst $ splitAt s l) len
    where len = [1..(length l)]
