import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T

app2 (f1,f2) (v1,v2) = (f1 v1, f2 v2)

parseForest :: T.Text -> [[Int]]
parseForest = map (unfoldr (\s -> app2 (digitToInt, id) <$> T.uncons s)) . T.lines

group2 a = zipWith const (init a) (tail a)

visibilityLeft :: [[Int]] -> [[Bool]]
visibilityLeft = map $ (snd . mapAccumL f (-1))
  where f acc tree = (max acc tree, tree > acc)

visibilityRight :: [[Int]] -> [[Bool]]
visibilityRight = map reverse . visibilityLeft . map reverse

visibilityTop :: [[Int]] -> [[Bool]]
visibilityTop = transpose . visibilityLeft . transpose

visibilityBottom :: [[Int]] -> [[Bool]]
visibilityBottom = transpose . map reverse . visibilityLeft . map reverse . transpose

-- i.e. foldl but over function arguments
chain4 :: (a -> a -> a) -> a -> a -> a -> a -> a
chain4 f a b c d = f a (f b (f c d))

visibilityAll forest = zipWith4 (zipWith4 f) l r t b
  where l = visibilityLeft forest
        r = visibilityRight forest
        t = visibilityTop forest
        b = visibilityBottom forest
        f = chain4 (||)

count pred = length . filter pred
countValue v = count (== v)

part1 :: T.Text -> Int
part1 = sum . map (countValue True) . visibilityAll . parseForest

part2 :: T.Text -> Int
part2 inp = 0

parts filePath = do
    inp <- T.readFile filePath
    putStrLn $ "Part 1: " ++ (show $ part1 inp)
    putStrLn $ "Part 2: " ++ (show $ part2 inp)

main = parts "inputs/day08.txt"
