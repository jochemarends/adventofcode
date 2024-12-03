import Data.Char
import Data.List (isPrefixOf)

data Token 
    = Mul
    | Number String 
    | Comma 
    | LParen 
    | RParen
    deriving (Show, Eq)

data Expr = Times Int Int deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(x:xs)
    | x == ',' = Comma : tokenize xs
    | x == '(' = LParen : tokenize xs
    | x == ')' = RParen : tokenize xs
    | isDigit x = let (digits, other) = span isDigit (x:xs)
        in Number digits : tokenize other
    | isPrefixOf "mul" s = Mul : tokenize (drop 3 s)
    | otherwise = tokenize xs

parse :: [Token] -> [Expr]
parse [] = []
parse (Mul:Number a:Number b:_) = [Times (read a) (read b)]
parse (_:xs) = parse xs

main :: IO ()
main = do
    input <- readFile "./input.txt"
    print $ parse $ tokenize input
    print input
    -- printf "part 1: %d\n" (part1 input)
