import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as VU

app2 (f1,f2) (v1,v2) = (f1 v1, f2 v2)

parseForestDim :: T.Text -> (Int, Int)
parseForestDim s = (length lines, T.length (head lines))
  where lines = T.lines s

parseForest :: T.Text -> VU.Vector Int
parseForest = VU.unfoldr f
  where f str = T.uncons str >>= h
        h (c, rest) = if isDigit c
                      then Just (digitToInt c, rest)
                      -- Keep iterating until we find the next digit (or exhausted input and give Nothing)
                      else f rest

generate2D width height f = VU.generate (width * height) (\i -> f (i `rem` width) (i `quot` width))

part1 inp = 0

part2 inp = 0

parts filePath = do
    inp <- T.readFile filePath
    putStrLn $ "Part 1: " ++ (show $ part1 inp)
    putStrLn $ "Part 2: " ++ (show $ part2 inp)

main = parts "inputs/day08.txt"
