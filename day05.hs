import Data.Char
import Data.Either
import Data.List
import Data.Tuple
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

dropIf pred xs = [x | x <- xs, not (pred x)]

parseCrateStacks :: T.Text -> [[Char]]
parseCrateStacks =
    map T.unpack
  . dropIf T.null
    -- Get rid of brackets and spaces
  . map (T.filter isUpper)
  . T.transpose
  . T.lines

tuplify2 [x,y] = (x,y)
tuplify3 [x,y,z] = (x,y,z)

panicError :: Show a => Either a b -> b
panicError (Left e) = error ("unexpected Left" ++ (show e))
panicError (Right v) = v

parseInstructions :: T.Text -> [(Int, Int, Int)]        
parseInstructions =
    map ( tuplify3
        . map (fst . panicError . T.decimal)
          -- ["1", "2", "3"]
        . filter (not . T.null)
          -- ["1", "", "2", "", "3"]
        . T.splitOn (T.singleton ' ')
          -- " 1  2  3"
        . T.filter (\x -> isNumber x || isSpace x)
          -- "move 1 from 2 to 3"
        )
  . T.lines

operateElements :: (a -> a -> (a, a)) -> Int -> Int -> [a] -> [a]
operateElements func n m list
  | n < m = operate func n m list
  | n > m = operate (\a b -> swap (func b a)) m n list
  | n == m = error "Cannot operate on the same element"
  where operate func n m list =
          let (front, rest) = splitAt n list
              (middle, rest') = splitAt (m - n - 1) (tail rest)
              back = tail rest'
              (leftRes, rightRes) = func (head rest) (head rest')
          in front ++ [leftRes] ++ middle ++ [rightRes] ++ back

insSrc (_,x,_) = x
insDst (_,_,x) = x
insCount (x,_,_) = x

applyIns :: ([Char] -> [Char]) -> [[Char]] -> (Int, Int, Int) -> [[Char]]
applyIns craneLifter stacks ins = operateElements handler isrc idst stacks
  where count = insCount ins
        -- Instruction indices are 1-indexed, convert to 0-index for operation on lists
        isrc = insSrc ins - 1
        idst = insDst ins - 1
        handler fromPile toPile = (fromPile', toPile')
          where fromPile' = drop count fromPile
                toPile' = (craneLifter $ take count fromPile) ++ toPile

app (f1,f2) (v1,v2) = (f1 v1, f2 v2)

preprocessInput :: T.Text -> (T.Text, T.Text)
preprocessInput inp = (crateStacks, instructions)
  where [p1, p2] = T.splitOn (T.pack "\n\n") inp
        crateStacks = (T.stripEnd . fst . T.breakOnEnd (T.pack "\n")) p1
        instructions = p2

extractTopCrates :: [[Char]] -> [Char]
extractTopCrates = map head

part1 = extractTopCrates . uncurry (foldl $ applyIns reverse) . app (parseCrateStacks, parseInstructions) . preprocessInput

part2 = extractTopCrates . uncurry (foldl $ applyIns id) . app (parseCrateStacks, parseInstructions) . preprocessInput

parts filePath = do
    inp <- T.readFile filePath
    putStrLn $ "Part 1: " ++ (show $ part1 inp)
    putStrLn $ "Part 2: " ++ (show $ part2 inp)

main = parts "inputs/day05.txt"
