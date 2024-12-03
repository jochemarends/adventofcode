import Data.Char
import Data.List (isPrefixOf)

data Token 
    = Mul
    | Number String 
    | Comma 
    | LParen 
    | RParen
    deriving (Show, Eq)

data Expr 
    = Term Expr Expr
    | Primary Token
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize all@(x:xs)
    | x == ',' = Comma : tokenize xs
    | x == '(' = LParen : tokenize xs
    | x == ')' = RParen : tokenize xs
    | isDigit x = let (digits, other) = span isDigit (x:xs)
        in Number digits : tokenize other
    | isPrefixOf "mul" all = Mul : tokenize (drop 3 all)
    | otherwise = tokenize xs

parse :: [Token] -> [Expr]
parse [] = []
parse [Number x] = [Primary (read x)]
parse (LParen:xs) = 
    let (before, after) = break (==RParen) xs
    in case after of
        (_:toks) -> parse toks
        [] -> error "no right paren was found"
parse (Mul:LParen:Number a:Comma:Number b:RParen:rest) = (parse [a]) * (parse b) + parse rest
parse (_:xs) = parse xs

main :: IO ()
main = do
    input <- readFile "./input.txt"
    print $ parse $ tokenize input
