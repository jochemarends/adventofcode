import Data.List
import System.IO
import Text.Printf

split :: [Int] -> ([Int], [Int])
split input
    | odd (length input) = error "the list's length is not even!"
    | otherwise = (evens, odds)
    where
        indices = zip [0..] input
        evens = [x | (idx, x) <- indices, even idx]
        odds  = [x | (idx, x) <- indices, odd  idx]
    
distance :: (Int, Int) -> Int
distance (a, b) = abs $ a - b

part1 :: [Int] -> Int
part1 input = 
    let (a, b) = split input 
    in sum $ map distance $ zip (sort a) (sort b)

count :: Eq a => a -> [a] -> Int
count needle haystack = length $ filter (needle==) haystack

part2 :: [Int] -> Int
part2 input = 
    let (a, b) = split input 
    in sum $ map (\elem -> elem * count elem b) a 

main :: IO ()
main = do
    handle <- openFile "./input.txt" ReadMode
    contents <- hGetContents handle
    let input = map read (words contents) :: [Int]
    printf "part 1: %d\n" (part1 input)
    printf "part 2: %d\n" (part2 input)
    hClose handle

