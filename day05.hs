import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

parseCrateSacks :: T.Text -> [[Char]]
parseCrateSacks =
    map T.unpack
  . T.transpose
    -- Get rid of brackets and spaces
  . map (T.filter isUpper)
  . T.lines

parseInstruction :: T.Text -> (Int, Int, Int)
parseInstruction str = case result of
    Left _ -> error "malformed input"
    Right ins -> ins
    where result = do
        Right (a, rest) <- T.decimal str
        Right (b, rest') <- T.decimal rest
        Right (c, rest'') <- T.decimal rest'
        return (a, b, c)

parseInstructions :: T.Text -> [(Int, Int, Int)]        
parseInstructions =
    map parseInstruction
    -- "1 2 3"
  . map (T.filter \x -> isNumber x || isSpace x)
    -- "move 1 from 2 to 3"
  . T.lines

executeStep :: [[a]] -> (Int, Int, Int) -> [[a]]
executeStep stack step = 
    let (count, fromPile, toPile) = step,
        splitAt count stack!!fromPile
    --where 

part1 inp = 0

part2 inp = 0

main = do
    lines <- (fmap T.lines . T.readFile) "inputs/day04.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 lines)
    putStrLn $ "Part 2: " ++ (show $ part2 lines)