import System.Environment
import qualified Data.Map as Map
import Data.Maybe
import Data.List

main = do
    args <- getArgs
    fileStr <- readFile $ head args
    let 
        commandsStr = lines fileStr
        cmdObjs = map rawInputToCMD commandsStr
        resMap = foldr processing (Map.empty,0) $ reverse cmdObjs
    putStrLn $ show resMap


processing :: CMD -> (Map.Map String Int, Int) -> (Map.Map String Int, Int)
processing cmd (m, h) = (newM, max h (highestInMap newM) )
    where newM = calculate cmd m

data CMD = CMD { reg :: String,
                 op :: String,
                 value :: Int,
                 condReg :: String,
                 condType :: String,
                 condValue :: Int } deriving (Show)

rawInputToCMD :: String -> CMD 
rawInputToCMD str = createCMD $ (words str) \\ ["if"]
    where createCMD = \ [r, o, v, cr, ct, cv] -> CMD r o (read v :: Int) cr ct (read cv :: Int)


highestInMap :: Map.Map String Int -> Int
highestInMap ma
  | length ( Map.toList ma ) > 0 = maximum $ map snd ( Map.toList ma)
  | otherwise = 0


calculate :: CMD -> Map.Map String Int-> Map.Map String Int
calculate cmd regs  
        | satisfied = Map.insert (reg cmd) newRValue regs
        | otherwise = regs
    
    where findRegValue = Map.lookup condR regs
          curCondRegValue 
            | findRegValue == Nothing = 0
            | otherwise = fromJust findRegValue 
          condR = condReg cmd
          oldRfind = Map.lookup (reg cmd) regs
          oldRValue 
            | oldRfind == Nothing = 0
            | otherwise = fromJust oldRfind
          newRValue = executeOp oldRValue (op cmd) (value cmd)
          satisfied = checkSatisfied curCondRegValue (condType cmd) (condValue cmd)

checkSatisfied :: Int -> String -> Int -> Bool
checkSatisfied reg "<" desired = reg < desired
checkSatisfied reg "==" desired = reg == desired
checkSatisfied reg ">" desired = reg > desired
checkSatisfied reg "!=" desired = reg /= desired
checkSatisfied reg "<=" desired = reg <= desired
checkSatisfied reg ">=" desired = reg >= desired



executeOp :: Int -> String -> Int -> Int
executeOp o "inc" v = o + v
executeOp o "dec" v = o - v
