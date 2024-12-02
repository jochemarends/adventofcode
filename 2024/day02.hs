import Data.List
import System.IO
import Text.Printf
import Data.Char

isSuperSafe :: [Int] -> Bool
isSuperSafe report = 
    let diffs = zipWith (-) report (tail report)
        signs = map signum diffs
    in all (\n -> n >= 1 && n <= 3) (map abs diffs) && all (==head signs) (tail signs)

part1 :: [[Int]] -> Int
part1 input = length $ filter isSuperSafe input

removeIndex :: Int -> [Int] -> [Int]
removeIndex idx lst = 
    let (a, b) = splitAt idx lst
    in a ++ tail b

isSafe :: [Int] -> Bool
isSafe report = any isSuperSafe $ map (\idx -> removeIndex idx report) [0..length report - 1]

part2 :: [[Int]] -> Int
part2 input = length $ filter isSafe input

main :: IO ()
main = do
    input <- map (map read . words) . lines <$> readFile "./input.txt"
    print input
    printf "part 1: %d\n" (part1 input)
    printf "part 2: %d\n" (part2 input)
