import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T

-- NOTE: doesn't handle haystack == [] properly
-- NOTE: produces empty lists in output if haystack has needle on edges
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn needle haystack =
    let (firsts, rest) = break (== needle) haystack
    in
        if null rest
        then [firsts]
        else firsts : splitOn needle (tail rest)

readInt :: T.Text -> Int
readInt t = case T.decimal t of
    Left _ -> error "Parse elf's calorgies failed"
    Right (n, _) -> n

part1 = do
    -- https://stackoverflow.com/a/22548591
    inpLines <- fmap T.lines (T.readFile "inputs/day01.txt")
    let inpElvesText = splitOn (T.pack "") inpLines
    let inpElvesCalories = fmap (fmap sum (fmap readInt)) inpElvesText
    return $ maximum inpElvesCalories

-- https://stackoverflow.com/a/2097903
removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

part2 = do
    inpLines <- fmap T.lines (T.readFile "inputs/day01.txt")
    let lst1 = fmap sum (fmap readInt) <$> splitOn (T.pack "") inpLines
    let elf1 = maximum lst1
    let lst2 = removeItem elf1 lst1
    let elf2 = maximum lst2
    let lst3 = removeItem elf2 lst2
    let elf3 = maximum lst3
    return (elf1 + elf2 + elf3)

main = do
    part1 <- part1
    putStrLn $ "Part 1: " ++ show part1
    part2 <- part2
    putStrLn $ "Part 2: " ++ show part2
