
import Data.Char (isDigit, digitToInt)
import System.Environment (getArgs)
import Data.List (foldl')
import qualified Data.Map as Map




-- Creating type Token with Number Constructor & Operator Constructor
data Token = Number Int | Operator Char deriving (Show,Eq)
type Operator = Char

-- Define the precedence of each Operator
precedence :: Operator -> Int
precedence '+' = 1
precedence '-' = 1
precedence '*' = 2
precedence '/' = 2
precedence _   = 0


-- Tokenize a String
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | isDigit c = let (digits, rest) = span isDigit (c:cs)
                  in Number (read digits) : tokenize rest
    | c `elem` "+-*/" = Operator c : tokenize cs
    | c `elem` "() " = tokenize cs
    | otherwise = error ("Unknown character: " ++ [c])



-- Convert a classic expression in Reverse Polish Notation
shuntingYard :: [Token] -> [Token]
shuntingYard = reverse . fst . foldl' parse ([], [])
  where
    parse (output, ops) (Number n) = (Number n : output, ops)
    parse (output, ops) (Operator o1) =
        let (output', ops') = popWhile ((<= precedence o1) . precedenceOp) output ops
        in (output', Operator o1 : ops')
    
    precedenceOp (Operator o) = precedence o
    precedenceOp _            = -1

    popWhile :: (Token -> Bool) -> [Token] -> [Token] -> ([Token], [Token])
    popWhile p output (o:ops)
        | p o = popWhile p (o:output) ops
    popWhile _ output ops = (output, ops)



-- Computing a Reverse Polish Notation
evaluateRPN :: [Token] -> Int
evaluateRPN = head . foldl' eval []
  where
    eval (x:y:ys) (Operator '+') = (y + x) : ys
    eval (x:y:ys) (Operator '-') = (y - x) : ys
    eval (x:y:ys) (Operator '*') = (y * x) : ys
    eval (x:y:ys) (Operator '/') = (y `div` x) : ys
    eval xs (Number n) = n : xs
    eval _ _ = error "Invalid RPN expression"



-- main loop
main :: IO Int
main = do
    let expression = "3 + 5 * 2 - 8 / 4"
    putStrLn $ "starting for expression : "++ expression
    let tokens = tokenize expression
    putStrLn $ "Tokenized expression: " ++ show tokens
    let rpn = shuntingYard tokens
    putStrLn $ "RPN: " ++ show rpn
    let result = evaluateRPN rpn
    print result
    return result