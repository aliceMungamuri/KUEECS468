{-
EECS 468 Assignment 8 - Arithmetic Expression Evaluator
Description: Parses and evaluates infix arithmetic expressions with parentheses, PEMDAS order, and error handling.
Inputs: String representing an arithmetic expression
Output: Float result or specific error messages
Sources: ChatGPT, StackOverflow (for general structure inspiration)
Collaborators: None
Author: Alice J Mungamuri
Creation Date: April 26, 2025
-}

-- Define a type for tokens: numbers, operators, parentheses
data Token = Number Float | Operator String | LParen | RParen deriving (Show, Eq)

-- Tokenize the input string into numbers, operators, parentheses
tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c:cs)
    | c == ' ' = tokenize cs  -- Skip spaces
    | c == '(' = fmap (LParen :) (tokenize cs)
    | c == ')' = fmap (RParen :) (tokenize cs)
    | c `elem` "+-*/%" = fmap (Operator [c] :) (tokenize cs)
    | c == '*' && not (null cs) && head cs == '*' = fmap (Operator "**" :) (tokenize (tail cs)) -- handle **
    | isDigit c || c == '.' || (c == '-' && (null cs || isDigit (head cs))) = 
        let (numStr, rest) = span (\x -> isDigit x || x == '.' || x == '-') (c:cs)
        in case reads numStr :: [(Float, String)] of
            [(n, "")] -> fmap (Number n :) (tokenize rest)
            _         -> Left "Error: invalid number format"
    | otherwise = Left "Error: invalid character"
  where
    isDigit ch = ch >= '0' && ch <= '9'

-- Operator precedence: higher number = higher precedence
precedence :: String -> Int
precedence "**" = 4
precedence "*"  = 3
precedence "/"  = 3
precedence "%"  = 3
precedence "+"  = 2
precedence "-"  = 2
precedence _    = 0

-- Check if an operator is left-associative
isLeftAssoc :: String -> Bool
isLeftAssoc "**" = False -- Exponentiation is right associative
isLeftAssoc _    = True

-- Convert infix tokens to postfix tokens using the Shunting Yard algorithm
toPostfix :: [Token] -> Either String [Token]
toPostfix tokens = shunt tokens [] []
  where
    shunt [] [] output = Right (reverse output) -- no more input, empty stack
    shunt [] (op:ops) output = case op of
        LParen -> Left "Error: unmatched parentheses"
        _      -> shunt [] ops (op:output)
    shunt (t:ts) stack output = case t of
        Number _ -> shunt ts stack (t:output)
        LParen   -> shunt ts (LParen:stack) output
        RParen   -> case span (/= LParen) stack of
                        (toOutput, _:ops) -> shunt ts ops (reverse toOutput ++ output)
                        _ -> Left "Error: unmatched parentheses"
        Operator op1 -> shunt ts (pushOp op1 stack) output

    -- Helper function to push operator considering precedence
    pushOp op1 [] = [Operator op1]
    pushOp op1 (Operator op2:ops)
        | (isLeftAssoc op1 && precedence op1 <= precedence op2) || (not (isLeftAssoc op1) && precedence op1 < precedence op2) =
            Operator op2 : pushOp op1 ops
        | otherwise = Operator op1 : Operator op2 : ops
    pushOp op1 stack = Operator op1 : stack

-- Evaluate the postfix tokens
evalPostfix :: [Token] -> Either String Float
evalPostfix = eval []
  where
    eval [Number n] [] = Right n -- Done: one final result
    eval stack (Number n : ts) = eval (Number n : stack) ts
    eval (Number y : Number x : rest) (Operator op : ts) =
        case op of
            "+" -> eval (Number (x + y) : rest) ts
            "-" -> eval (Number (x - y) : rest) ts
            "*" -> eval (Number (x * y) : rest) ts
            "/" -> if y == 0 then Left "Error: division by zero" else eval (Number (x / y) : rest) ts
            "%" -> if y == 0 then Left "Error: division by zero" else eval (Number (fromIntegral (floor x `mod` floor y)) : rest) ts
            "**" -> eval (Number (x ** y) : rest) ts
            _   -> Left "Error: unknown operator"
    eval _ (Operator _ : _) = Left "Error: operator without operands"
    eval _ [] = Left "Error: malformed expression"
    eval _ _ = Left "Error: malformed expression"

-- Main function: parse an input string and return a Float result
parse :: String -> Float
parse s = case tokenize s of
    Left err -> error err
    Right tokens -> case toPostfix tokens of
        Left err -> error err
        Right post -> case evalPostfix post of
            Left err -> error err
            Right val -> val
