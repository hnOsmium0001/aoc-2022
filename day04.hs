import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

data Section = Section { begin :: Int
                       , end :: Int
                       } deriving (Show)

data ElfPair = ElfPair Section Section

parseIntOnly str =
    let Right (num, rest) = T.decimal str
    in num

parseSection str =
    let segments = (T.splitOn (T.pack "-") str)
        [begin, end] = map parseIntOnly segments
    in  Section begin end

parseElfPair str =
    let [elf1, elf2] = T.splitOn (T.pack ",") str
    in  (parseSection elf1, parseSection elf2)

sectionsContain :: Section -> Section -> Bool
sectionsContain
    -- Really ugly...
    Section{begin=beg1, end=end1}
    Section{begin=beg2, end=end2} = beg1 <= beg2 && end1 >= end2

part1 = length
    . filter (\(l, r) -> sectionsContain l r || sectionsContain r l)
    . map parseElfPair

sectionsOverlap :: Section -> Section -> Bool
sectionsOverlap a b =
    (begin a) <= (begin b) && (begin b) <= (end a) ||
    (begin a) > (begin b) && (begin a) <= (end b)
 
part2 = length
    . filter (uncurry sectionsOverlap)
    . map parseElfPair

main = do
    lines <- (fmap T.lines . T.readFile) "inputs/day04.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 lines)
    putStrLn $ "Part 2: " ++ (show $ part2 lines)