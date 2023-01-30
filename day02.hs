{-# LANGUAGE LambdaCase #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T

(<<-) :: (s, [r]) -> Maybe r -> (s, [r])
(<<-) (src, lst) Nothing = (src, lst)
(<<-) (src, lst) (Just val) = (src, val : lst)

(<<-.) :: (s, [r]) -> (s -> Maybe (s, r)) -> (s, [r])
(<<-.) (src, lst) fn = case fn src of
    Nothing -> (src, lst)
    Just (rest, val) -> (rest, val : lst)

parseCharFromText fn text = do
    (rest, c) <- T.unsnoc text
    return $ case T.unsnoc rest of
        Nothing -> (rest, fn c)
        Just (rrest, sp) -> if sp == ' ' then (rrest, fn c) else (rest, fn c)

part1ParseLine ln = snd $ (ln, [])
    -- Our strategy
    <<-. parseCharFromText (\case
        'X' -> 'R' -- Rock
        'Y' -> 'P' -- Paper
        'Z' -> 'S') -- Scissors
    -- Oppoent strategy
    <<-. parseCharFromText (\case
        'A' -> 'R' -- Rock
        'B' -> 'P' -- Paper
        'C' -> 'S') -- Scissors

baseScore 'R' = 1
baseScore 'P' = 2
baseScore 'S' = 3

roundScore ours theirs
    | ours == theirs = 3 + baseScore ours
    | ours == 'R' && theirs == 'S' = 6 + baseScore ours
    | ours == 'P' && theirs == 'R' = 6 + baseScore ours
    | ours == 'S' && theirs == 'P' = 6 + baseScore ours
    | otherwise = baseScore ours

part1 = do
    instructions <- fmap (map part1ParseLine. T.lines) (T.readFile "inputs/day02.txt")
    return $ foldl reducer 0 instructions
        where reducer acc (theirs:ours:_) = acc + roundScore ours theirs
              reducer acc _ = acc

part2ParseLine ln = snd $ (ln, [])
    -- Required outcome
    <<-. parseCharFromText (\case
        'X' -> 'L' -- Lose
        'Y' -> 'D' -- Draw
        'Z' -> 'W') -- Win
    -- Oppoent strategy
    <<-. parseCharFromText (\case
        'A' -> 'R' -- Rock
        'B' -> 'P' -- Paper
        'C' -> 'S') -- Scissors

-- Find what our action is needed to win this round
ourAction theirs 'W' = case theirs of
    'R' -> 'P'
    'P' -> 'S'
    'S' -> 'R'

-- Find what our action is needed to lose this round
ourAction theirs 'L' = case theirs of
    'R' -> 'S'
    'P' -> 'R'
    'S' -> 'P'

-- Find what our action is needed to draw this round
ourAction theirs 'D' = theirs

part2 = do
    instructions <- fmap (map part2ParseLine. T.lines) (T.readFile "inputs/day02.txt")
    return $ foldl reducer 0 instructions
        where reducer acc (theirs:outcome:_) = acc + roundScore (ourAction theirs outcome) theirs
              reducer acc _ = acc

main = do
    part1 <- part1
    putStrLn $ "Part 1: " ++ show part1
    part2 <- part2
    putStrLn $ "Part 2: " ++ show part2
