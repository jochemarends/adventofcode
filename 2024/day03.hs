import Data.Char
import Data.List (stripPrefix)
import Text.Printf

data Token 
    = Mul | Pause | Resume
    | Number String 
    | Comma 
    | LParen | RParen
    | Corrupt
    deriving (Show, Eq)

data Expr = Term Int Int deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize all@(x:xs)
    | x == ',' = Comma : tokenize xs
    | x == '(' = LParen : tokenize xs
    | x == ')' = RParen : tokenize xs
    | isDigit x = 
        let (digits, other) = span isDigit (x:xs)
        in if length digits `elem` [1..3]
        then Number digits : tokenize other
        else tokenize other
    | Just rest <- stripPrefix "mul" all = Mul : tokenize rest
    | Just rest <- stripPrefix "don't()" all = Pause : tokenize rest
    | Just rest <- stripPrefix "do()"  all = Resume : tokenize rest
    | otherwise = Corrupt : tokenize xs

data Mode = Part1 | Part2

parse :: Mode -> [Token] -> [Expr]
parse _ [] = []
parse Part2 (Pause:rest) = parse Part2 $ dropWhile (/=Resume) rest
parse Part2 (Resume:rest) = parse Part2 rest
parse mode (Mul:LParen:Number a:Comma:Number b:RParen:rest) = Term (read a) (read b) : parse mode rest
parse mode (x:xs) = parse mode xs

eval :: [Expr] -> [Int]
eval [] = []
eval (Term a b:rest) = a * b : eval rest

main :: IO ()
main = do
    input <- readFile "./input.txt"
    printf "part 1 %d\n" $ sum $ eval $ (parse Part1) $ tokenize input
    printf "part 2 %d\n" $ sum $ eval $ (parse Part2) $ tokenize input
