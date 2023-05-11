import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.IO as T

containsChar :: T.Text -> Char -> Bool
containsChar haystack ch = case T.uncons haystack of
    Nothing -> False
    Just (curr, rest) -> ch == curr || containsChar rest ch

-- NOTE: buggy, if the same char appears multiple times in either one of the haystacks, the char will be included multiple times in the answers
--       we are just assuming that there will only be one char that's duplicated across the compartments (per problem description)
-- NOTE: hence the use of `head` on the return value of this function in code below
findDuplicateChar :: T.Text -> T.Text -> String
findDuplicateChar hs1 hs2 = case T.uncons hs1 of
    Nothing -> ""
    Just (c, rest) ->
        if containsChar hs2 c
        then c : findDuplicateChar rest hs2
        else findDuplicateChar rest hs2

itemPriority c
    | n >= ord 'a' && n <= ord 'z' = n - ord 'a' + 1
    | n >= ord 'A' && n <= ord 'Z' = n - ord 'A' + 27
    | otherwise = 0
    where n = ord c

-- Parse a line describing a runsack into its two compartments
parseRunsack ln = T.splitAt (div (T.length ln) 2) ln

part1 = do
    runsacks <- fmap (map parseRunsack . T.lines) (T.readFile "inputs/day03.txt")
    let duplicateItems = map (head . uncurry findDuplicateChar) runsacks
        priorities = map itemPriority duplicateItems
    return $ sum priorities

-- Or an unholy long line of function compositions, if you like it that way
part1PointFree = fmap (sum . map (itemPriority . head . uncurry findDuplicateChar . parseRunsack) . T.lines) (T.readFile "inputs/day03.txt")

findBadgeItemRunsacks runsack1 runsack2 runsack3 =
    findDuplicateChar (T.pack (findDuplicateChar runsack1 runsack2)) runsack3

findBadgeItemGroup [x,y,z] = findBadgeItemRunsacks x y z

splitEvery _ [] = []
splitEvery n xs = first : (splitEvery n rest)
    where (first, rest) = splitAt n xs

part2 = do
    file <- T.readFile "inputs/day03.txt"
    let -- We don't care about compartments in part 2, just things carried by each elf
        runsacks = T.lines file
        groups = splitEvery 3 runsacks
        groupBadges = map findBadgeItemGroup groups
        groupPriorities = map (itemPriority . head) groupBadges
    return $ sum groupPriorities

main = do
    part1 <- part1
    putStrLn $ "Part 1: " ++ show part1
    part2 <- part2
    putStrLn $ "Part 2: " ++ show part2
