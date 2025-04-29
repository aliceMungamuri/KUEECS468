{-
EECS 468 Assignment 8 
Description: Goes through expression and executes them in PEMDAS order and if not valid it throws an error
Inputs: is the string that represents the expressions
Output: float result or error
Sources: Stack overflow - a little chatgpt
Alice J Mungamuri
Creation Date: April 25, 2025
-}

-- Define a type for tokens: numbers, operators, parentheses
data Token = Number Float | Operator String | LParen | RParen deriving (Show, Eq)

-- Tokenize the input string into numbers, operators, parentheses
tokenize :: String -> Either String [Token] -- takes input string and will break it up into a list of tokens
tokenize [] = Right [] -- base case in case its empty
tokenize (c:cs) 
    | c == ' ' = tokenize cs  -- thee are the skip cases if its a space ignore and move to the next charactear
    | c == '(' = fmap (LParen :) (tokenize cs) ---- if you see a left parantheses then you make a LParen token 
    | c == ')' = fmap (RParen :) (tokenize cs) -- if you see a right  parantheses then you make a RParen token
    | c `elem` "+-*/%" = fmap (Operator [c] :) (tokenize cs) -- operator token if its a basic operator character
    | c == '*' && not (null cs) && head cs == '*' = fmap (Operator "**" :) (tokenize (tail cs)) -- I looked this up basically it will handle exponentiation
    | isDigit c || c == '.' || (c == '-' && (null cs || isDigit (head cs))) =  -- if the char is a digit or a dot (because decimals) or a negative sign and then it builds a number
        let (numStr, rest) = span (\x -> isDigit x || x == '.' || x == '-') (c:cs) -- groups the number together
        in case reads numStr :: [(Float, String)] of -- this will convert the number string into a float 
            [(n, "")] -> fmap (Number n :) (tokenize rest) -- if it succeeds - makes the number token
            _         -> Left "Error: invalid number format" -- if its not in the right format the error is returned
    | otherwise = Left "Error: invalid character" --  from Chatgpt its how youre supposed  to throw an error
  where -- helper function
    isDigit ch = ch >= '0' && ch <= '9' -- check if a character is a digit

-- the diff procedures the higher num will be the higher priority in the PEMDAS 
precedence :: String -> Int -- 
precedence "**" = 4 -- exponentiation
precedence "*"  = 3 -- multiplication
precedence "/"  = 3 -- div is the same as multiplication
precedence "%"  = 3 -- modulo is the same
precedence "+"  = 2 -- addition and subtraction are lower
precedence "-"  = 2
precedence _    = 0 -- base case


isLeftAssoc :: String -> Bool -- checks if an operator is left associative 
isLeftAssoc "**" = False -- Exponentiation is right associative
isLeftAssoc _    = True -- everything else besides that will be left associative


toPostfix :: [Token] -> Either String [Token]-- Convert infix tokens to postfix tokens using the Shunting Yard algorithm
toPostfix tokens = shunt tokens [] [] -- will process with empty input tokens and empty operator stack 
  where
    shunt [] [] output = Right (reverse output) -- no more input, empty stack
    shunt [] (op:ops) output = case op of -- if input is empty but operator stack isnt 
        LParen -> Left "Error: unmatched parentheses" -- this is if theres a leftover parenthetical 
        _      -> shunt [] ops (op:output) -- otherwise itll move the operator to the output
    shunt (t:ts) stack output = case t of -- read it one at a tiem
        Number _ -> shunt ts stack (t:output) -- direct to output
        LParen   -> shunt ts (LParen:stack) output -- pushed onto the stack
        RParen   -> case span (/= LParen) stack of -- pop the stack until you fin the matching left parenthetical 
                        (toOutput, _:ops) -> shunt ts ops (reverse toOutput ++ output)
                        _ -> Left "Error: unmatched parentheses"
        Operator op1 -> shunt ts (pushOp op1 stack) output -- decide based on precedence whether to push or pop operators 

   
    pushOp op1 [] = [Operator op1]  -- Helper function to push operator considering precedence
    pushOp op1 (Operator op2:ops) -- if the top of the stack is an operator precedence and associativitivy to know whether o pop or push 
        | (isLeftAssoc op1 && precedence op1 <= precedence op2) || (not (isLeftAssoc op1) && precedence op1 < precedence op2) =
            Operator op2 : pushOp op1 ops
        | otherwise = Operator op1 : Operator op2 : ops
    pushOp op1 stack = Operator op1 : stack


evalPostfix :: [Token] -> Either String Float -- Evaluate the postfix tokens
evalPostfix = eval [] -- starter empty stack
  where
    eval [Number n] [] = Right n -- Done: one final result
    eval stack (Number n : ts) = eval (Number n : stack) ts -- if the next num is a token put onto stack
    eval (Number y : Number x : rest) (Operator op : ts) = -- if its an operator they pop the a and y numbers and use the operato ron them and thern push it back onto the stack
        case op of
            "+" -> eval (Number (x + y) : rest) ts -- these are the diff operations
            "-" -> eval (Number (x - y) : rest) ts
            "*" -> eval (Number (x * y) : rest) ts
            "/" -> if y == 0 then Left "Error: division by zero" else eval (Number (x / y) : rest) ts -- can't divide by 0
            "%" -> if y == 0 then Left "Error: division by zero" else eval (Number (fromIntegral (floor x `mod` floor y)) : rest) ts 
            "**" -> eval (Number (x ** y) : rest) ts
            _   -> Left "Error: unknown operator" -- anything besides these I didnt code so Ill throw this error
    eval _ (Operator _ : _) = Left "Error: operator without operands" -- not enough numbers = error
    eval _ [] = Left "Error: malformed expression" -- if theres leftover parenthese or anything elses thats random
    eval _ _ = Left "Error: malformed expression"


parse :: String -> Float -- this is the main parseing function
parse s = case tokenize s of -- these are for all of the errors and when they're thrown itll be a helpful message
    Left err -> error err
    Right tokens -> case toPostfix tokens of
        Left err -> error err
        Right post -> case evalPostfix post of
            Left err -> error err
            Right val -> val
