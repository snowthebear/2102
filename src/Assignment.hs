{-# OPTIONS_GHC -Wno-missing-export-lists #-}
    --This module contains the skeleton code for the assignment.
--
-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.
--
-- You may, and are highly encouraged, to create your functions.
module Assignment where

import Instances
import Parser
import Control.Applicative
import Data.List (intercalate, stripPrefix)
import Data.Char
import Debug.Trace --delete later


data ADT = IntLit Int
         | StringLit String
         | BoolLit Bool
         | List [ADT]
         | Add ADT ADT
         | Subtract ADT ADT
         | Multiply ADT ADT
         | Divide ADT ADT
         | And ADT ADT
         | Or ADT ADT
         | Not ADT
         | Ternary ADT ADT ADT
         | Equal ADT ADT        
         | NotEqual ADT ADT     
         | GreaterThan ADT ADT  
         | LessThan ADT ADT
         | Identifier String
         | Block [ADT] --B
         | Var String -- B
         | ConstDeclaration String ADT --B
         | ConstMultiDeclarations [ADT] --B
         | IfStatement ADT ADT --B
         | IfElseStatement ADT ADT ADT -- condition, if-body, else-body
         | Comment String --B
         | Program [ADT] -- B
         | ConstAndBlock [ADT] ADT --B
         | FunctionCall String [ADT] --C
         | EmbeddedFunction String [ADT] --C, this is a function call inside an expressions
         | BinaryOp String ADT ADT  -- C. String is the operation (+, -, etc.)
         | ReturnStatement ADT --C
         | FunctionStructure { --C
            functionName :: String,
            functionParams :: [String],
            functionBody :: [ADT]
         } 
         | WhileLoop ADT [ADT]
         | DestructureRecursion [String] [ADT]
         | WhileStatement ADT
         | Empty
  deriving (Eq, Show)

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p operator = do 
    initialVal <- p 
    continueParsing initialVal
  where
    continueParsing val = (do 
                opFunc <- operator
                nextVal <- p
                continueParsing (opFunc val nextVal)
             ) <|> pure val

indent :: String -> String
indent = unlines . map ("    " ++) . lines

indentForWhile :: String -> String 
indentForWhile = unlines . map ("        " ++) . lines

isTok :: Char -> Parser Char
isTok c = tok (is c)

openParen :: Parser Char
openParen = op '('

closeParen :: Parser Char
closeParen = op ')' 

op :: Char -> Parser Char
op c = spaces *> is c <* spaces

opStr :: String -> Parser String
opStr s = spaces *> string s <* spaces

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x:xs)

quoteString :: Parser String
quoteString = is '"' *> many (isNot '"') <* isTok '"'

parseExpr :: Parser ADT
parseExpr =
        parseFunctionStructure
        <|> parseBinaryOp
        <|> parseTernary 
        <|> parseOr 
        <|> parseAnd 
        <|> parseNot
        <|> parseComparison 
        <|> parseFunctionCall
        <|> parseArithmetic 
        <|> parseEmbeddedFunction
        <|> parseConstDeclaration
        <|> parseElement
        <|> parseAllStatements

-- | Exercise A
parseInt :: Parser ADT
parseInt =  IntLit <$> ((op '-' *> (negate <$> int)) <|> int)

-- parser for string
parseString :: Parser ADT
parseString = do
    _ <- spaces  -- for leading spaces
    str <- StringLit <$> quoteString
    _ <- spaces  -- for trailing spaces
    return str

-- parser for Boolean literals
parseBool :: Parser ADT
parseBool =
    (opStr "true" *> pure (BoolLit True)) <|> 
    (opStr "false" *> pure (BoolLit False))


parseList :: Parser ADT
parseList = List <$> (spaces *> op '[' *> spaces *> sepBy parseElement (spaces *> op ',' <* spaces) <* spaces <* op ']' <* spaces)


surroundedBy :: Parser a -> Parser b -> Parser a -> Parser b
surroundedBy open p close = do
    _ <- open
    x <- p
    _ <- close
    return x

        

parseOr :: Parser ADT
parseOr = 
    openParen *> parseExpr <* opStr "||" <* spaces
    >>= \expr1 -> parseExpr <* closeParen
    >>= \expr2 -> pure $ Or expr1 expr2


parseAnd :: Parser ADT
parseAnd = 
    openParen *> parseExpr <* opStr "&&"
    >>= \expr1 -> parseExpr <* closeParen
    >>= \expr2 -> pure $ And expr1 expr2

parseNot :: Parser ADT
parseNot = 
    openParen *> tok (opStr "!") *> parseExpr <* closeParen
    >>= \expr -> pure $ Not expr

    
parseEqual :: Parser ADT
parseEqual = 
    openParen *> parseExpr <* opStr "==="
    >>= \expr1 -> parseExpr <* closeParen
    >>= \expr2 -> pure $ Equal expr1 expr2


parseComparison :: Parser ADT
parseComparison = 
    openParen *> parseElement <* spaces >>= \expr1 -> 
    (   (opStr "===" *> pure Equal)
    <|> (opStr "!==" *> pure NotEqual)
    <|> (op '>' *> pure GreaterThan)
    <|> (op '<' *> pure LessThan)   ) <* spaces >>= \compOp -> 
    parseElement <* closeParen
    >>= \expr2 -> 
    pure $ compOp expr1 expr2

parseTernary :: Parser ADT
parseTernary = 
    openParen *> parseExpr <* opStr "?"
    >>= \condition -> parseExpr <* opStr ":"
    >>= \trueBranch -> parseExpr <* closeParen
    >>= \falseBranch -> pure $ Ternary condition trueBranch falseBranch


parseAdd :: Parser ADT
parseAdd = chain parseTerm (op '+' *> pure Add)

parseSub :: Parser ADT
parseSub = chain parseTerm (op '-' *> pure Subtract)

parseMul :: Parser ADT
parseMul = chain parseFactor (op '*' *> pure Multiply)

parseDiv :: Parser ADT
parseDiv = chain parseFactor (op '/' *> pure Divide)


parseBoolExpr :: Parser ADT
parseBoolExpr = parseOr <|> parseAnd <|> parseBool

parseFactor :: Parser ADT
parseFactor = parseInt
          <|> surroundedBy openParen parseExpr closeParen
          <|> parseEmbeddedFunction
          <|> parseIdentifier
          


parseTerm :: Parser ADT
parseTerm = chain parseFactor
                 ((op '*' *> pure Multiply)
              <|> (op '/' *> pure Divide))

parseArithmetic :: Parser ADT
parseArithmetic = chain parseTerm 
                      ((op '+' *> pure Add)
                   <|> (op '-' *> pure Subtract))


parseElement :: Parser ADT
parseElement = parseInt 
           <|> parseString 
           <|> parseBool 
           <|> parseNot
           <|> parseList
           <|> surroundedBy openParen parseExpr closeParen
           <|> parseIdentifier

parseIdentifier :: Parser ADT
parseIdentifier = do
    name <- identifier
    pure $ Identifier name



multiline :: String -> Bool
multiline str = length str > 42



shouldPrintMultiline :: ADT -> Bool
shouldPrintMultiline block@(Ternary cond trueBranch falseBranch) = 
    multiline(prettyPrintExerciseA cond ++ " ? " ++ prettyPrintExerciseA trueBranch ++ ":" ++ prettyPrintExerciseA falseBranch)
    || anyChildIsMultiline block
shouldPrintMultiline block@(IfStatement cond trueBranch) =
    multiline("if ( " ++ prettyPrintExerciseB cond ++ " ) " ++ prettyPrintBlock trueBranch)
    || anyChildIsMultiline block
shouldPrintMultiline block@(IfElseStatement cond trueBranch falseBranch) = 
    multiline("if ( " ++ prettyPrintExerciseB cond ++ " ) " ++ prettyPrintBlock trueBranch ++ " else " ++ prettyPrintBlock falseBranch)
    || totalElements block > 1
    || totalElements block > 1    
    || anyChildIsMultiline block
shouldPrintMultiline _ = False


-- printMultiline :: ADT -> String
-- printMultiline (Ternary cond trueBranch falseBranch) =
--     "(" ++ prettyPrintExerciseA cond ++ "\n? " ++ 
--     prettyPrintExerciseA trueBranch ++ "\n: " ++ 
--     prettyPrintExerciseA falseBranch ++ ")"
-- printMultiline (IfElseStatement condition trueBranch falseBranch) = 
--     "if ( " ++ prettyPrintExerciseA condition ++ " ) "  ++ prettyPrintBlock trueBranch ++ " else " ++ prettyPrintBlock falseBranch



anyChildIsMultiline :: ADT -> Bool
anyChildIsMultiline (Block stmts) = any shouldPrintMultiline stmts
anyChildIsMultiline (Ternary _ trueBranch falseBranch) = shouldPrintMultiline trueBranch || shouldPrintMultiline falseBranch
anyChildIsMultiline (IfStatement _ trueBranch) = shouldPrintMultiline trueBranch
anyChildIsMultiline (IfElseStatement _ trueBranch falseBranch) = shouldPrintMultiline trueBranch || shouldPrintMultiline falseBranch
anyChildIsMultiline _ = False





parseExerciseA :: Parser ADT
parseExerciseA = parseExpr

prettyPrintExerciseA :: ADT -> String 
prettyPrintExerciseA (Identifier name) = name
prettyPrintExerciseA (IntLit n) = show n
prettyPrintExerciseA (StringLit s) = "\"" ++ s ++ "\""
prettyPrintExerciseA (BoolLit b) = if b then "true" else "false"
prettyPrintExerciseA (List adts) = "[" ++ intercalate ", " (map prettyPrintExerciseA adts) ++ "]"
prettyPrintExerciseA (Add e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " + " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (Subtract e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " - " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (Multiply e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " * " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (Divide e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " / " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (And e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " && " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (Or e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " || " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (Not e) = "(!" ++ prettyPrintExerciseA e ++ ")"
prettyPrintExerciseA (Equal e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " === " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (NotEqual e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " !== " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (GreaterThan e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " > " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (LessThan e1 e2) = "(" ++ prettyPrintExerciseA e1 ++ " < " ++ prettyPrintExerciseA e2 ++ ")"
prettyPrintExerciseA (Ternary cond trueBranch falseBranch) 
    | shouldPrintMultiline (Ternary cond trueBranch falseBranch) = 
        "(" ++ prettyPrintExerciseA cond ++ "\n? " ++ 
        prettyPrintExerciseA trueBranch ++ "\n: " ++ 
        prettyPrintExerciseA falseBranch ++ ")"
    | otherwise = "(" ++ prettyPrintExerciseA cond ++ " ? " ++ prettyPrintExerciseA trueBranch ++ " : " ++ prettyPrintExerciseA falseBranch ++ ")"
  
prettyPrintExerciseA Empty = ""
prettyPrintExerciseA _ = ""


--------------------------------------------------------------------------------------------------------------------------------------------------

-- | Exercise B

openCurly :: Parser Char
openCurly = op '{'

closeCurly :: Parser Char
closeCurly = op '}' 

-- Exercise 1
identifier :: Parser String
identifier = do
    id <- some (alpha <|> digit <|> is '_')
    if id `elem` reservedWords
    then empty
    else return id

semicolon :: Parser Char
semicolon = is ';'

reservedWords :: [String]
reservedWords = ["if", "else", "const", "function", "return", "while", "for", "true", "false"]

parseSemicolon :: Parser Char
parseSemicolon = spaces *> op ';' <* spaces

parseVar :: Parser ADT
parseVar = do
    v <- many1 (alpha <|> digit)
    if v `elem` reservedWords
    then empty
    else pure $ Var v


parseConstDeclaration :: Parser ADT
parseConstDeclaration = do
    _ <- opStr "const"
    varName <- identifier
    _ <- spaces *> op '=' <* spaces
    expr <- parseEmbeddedFunction <|> parseExpr  
    _ <- optional spaces  -- consume trailing spaces after the expression, if any
    sem <- parseSemicolon
    pure $ ConstDeclaration varName expr

parseMultipleConstDeclarations :: Parser ADT
parseMultipleConstDeclarations = do
    declarations <- sepBy1 parseConstDeclaration (spaces *> semicolon <* spaces) -- Using sepBy1 to ensure at least one declaration
    pure $ ConstMultiDeclarations declarations


prettyPrintConstDeclaration :: ADT -> String
prettyPrintConstDeclaration (ConstDeclaration varName expr) = 
    "const " ++ varName ++ " = " ++ trim (prettyPrintExerciseA expr) ++ ";"

prettyPrintMultipleConstDeclarations :: [ADT] -> String
prettyPrintMultipleConstDeclarations adts = 
    intercalate "\n" $ map prettyPrintConstDeclaration adts


-- Exercise 2

isEmptyStatement :: ADT -> Bool
isEmptyStatement (StringLit s) = all isSpace s
isEmptyStatement Empty = True
isEmptyStatement _ = False

isConstDeclaration :: ADT -> Bool
isConstDeclaration (ConstDeclaration _ _) = True
isConstDeclaration _ = False


parseIf :: Parser ADT
parseIf = do
    _ <- opStr "if"
    _ <- op '('
    condition <- parseExerciseA
    _ <- op ')'
    trueBranch <- parseBlock
    falseBranch <- (opStr "else" *> parseBlock) <|> pure Empty
    case falseBranch of
        Empty -> pure $ IfStatement condition trueBranch
        _     -> pure $ IfElseStatement condition trueBranch falseBranch



-- parseElse :: Parser ADT
-- parseElse = do
--     _ <- opStr "else"
--     surroundedBy openCurly parseBlock closeCurly


parseBlock :: Parser ADT
parseBlock = do
    _ <- openCurly
    statements <- many (spaces *> parseStatement <* spaces <* optional parseSemicolon <* spaces)
    _ <- closeCurly
    pure $ Block statements
    


shouldPrintMultilineIfElse :: ADT -> Bool
shouldPrintMultilineIfElse (IfElseStatement _ trueBranch falseBranch) =
    totalElements trueBranch > 1 || totalElements falseBranch > 1
shouldPrintMultilineIfElse _ = False


totalElements :: ADT -> Int -- Helper to count the total elements
totalElements (Block stmts) = sum $ map totalElements stmts
totalElements (IfElseStatement _ trueBranch falseBranch) = 
    1 + totalElements trueBranch + totalElements falseBranch
totalElements (IfStatement _ trueBranch) = 1 + totalElements trueBranch
totalElements _ = 1



prettyPrintBlock :: ADT -> String
prettyPrintBlock (Block []) = "{ }"
prettyPrintBlock block@(Block [innerBlock@(Block _)]) = prettyPrintBlock innerBlock
prettyPrintBlock block@(Block stmts)
    | not (shouldPrintMultilineIfElse block) && totalElements block > 1 =
        "{ \n" ++ indent (intercalate "\n" $ map (trim . prettyPrintExerciseBForBlock) stmts) ++ " }"
    | otherwise = wrapWithBraces (intercalate "\n" $ map (trim . prettyPrintExerciseBForBlock) stmts)

prettyPrintBlock (Var varName) = varName
prettyPrintBlock (ReturnStatement expr) = "return " ++ prettyPrintExerciseC expr ++ ";"
prettyPrintBlock (ConstDeclaration varName expr) = "const " ++ varName ++ " = " ++ trim (prettyPrintExerciseA expr) ++ ";"
prettyPrintBlock (ConstMultiDeclarations adts) = intercalate ";\n" $ map prettyPrintConstDeclaration adts
prettyPrintBlock stmt@(IfStatement condition trueBranch) = 
    "if ( " ++ trim (prettyPrintExerciseA condition) ++ " ) " ++ prettyPrintBlock trueBranch
prettyPrintBlock stmt@(IfElseStatement condition trueBranch falseBranch) 
    | shouldPrintMultilineIfElse stmt =
        "if ( " ++ trim (prettyPrintExerciseA condition) ++ " ) {\n" ++ indent (prettyPrintBlock trueBranch) ++ "\n} else {\n" ++ indent (prettyPrintBlock falseBranch) ++ "\n}"
    | otherwise = 
        "if ( " ++ trim (prettyPrintExerciseA condition) ++ " ) " ++ prettyPrintBlock trueBranch ++ " else " ++ prettyPrintBlock falseBranch
prettyPrintBlock (Program adts) = intercalate "\n" $ map prettyPrintExerciseB adts



prettyPrintExerciseBForBlock :: ADT -> String
prettyPrintExerciseBForBlock (ConstMultiDeclarations adts) = 
    intercalate "\n" $ map prettyPrintConstDeclaration adts
prettyPrintExerciseBForBlock adt = prettyPrintExerciseB adt

-- Exercise 3

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


wrapWithBraces :: String -> String
wrapWithBraces str
    | '\n' `elem` str = "{\n" ++ indent str ++ "} "
    | otherwise      = "{ " ++ str ++ " }"



many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p


parseConstAndBlock :: Parser ADT
parseConstAndBlock = do
    declarations <- many1 (parseConstDeclaration <* spaces <* optional parseSemicolon <* spaces)
    stmtOrBlock <- optional (parseBlock <|> parseStatementAfterConst)
    case stmtOrBlock of
        Just (Block stmts) -> return $ ConstAndBlock declarations (Block $ declarations ++ stmts)
        Just s  -> return $ ConstAndBlock declarations s
        Nothing -> return $ ConstMultiDeclarations declarations



parseConstsThenIf :: Parser ADT
parseConstsThenIf = do
    decls <- many1 (parseConstDeclaration <* spaces <* optional parseSemicolon <* spaces)
    stmt <- parseIf
    pure $ ConstAndBlock decls stmt

parseStatementAfterConst :: Parser ADT
parseStatementAfterConst = parseBlock
                       <|> parseIf
                       <|> parseMultipleConstDeclarations 
                       <|> parseConstDeclaration
                       <|> parseVar

parseStatement :: Parser ADT
parseStatement = parseReturn
            -- <|> parseBlock
            <|> parseIf
            <|> parseConstsThenIf
            <|> parseConstAndBlock
            <|> parseMultipleConstDeclarations 
            <|> parseConstDeclaration
            <|> parseVar 
            <|> parseEmbeddedFunction
            <|> parseFullExpression
            <|> parseFunctionCall

parseAllStatements :: Parser ADT
parseAllStatements = do
    statements <- many1 parseStatement
    return $ Program statements

prettyPrintConstsAndStatement :: ADT -> String
prettyPrintConstsAndStatement (ConstAndBlock decls stmt) =
    intercalate "\n" (map prettyPrintConstDeclaration decls) ++ "\n" ++ prettyPrintExerciseB stmt


parseExerciseB :: Parser ADT
parseExerciseB = parseAllStatements


prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (Var varName) = varName
prettyPrintExerciseB block@(Block _) = prettyPrintBlock block
prettyPrintExerciseB (Program adts) = intercalate "\n" $ map prettyPrintExerciseB adts
prettyPrintExerciseB (ConstDeclaration varName expr) = "const " ++ varName ++ " = " ++ trim (prettyPrintExerciseA expr) ++ ";"
prettyPrintExerciseB (ConstMultiDeclarations adts) = 
    intercalate ";\n" $ map prettyPrintConstDeclaration adts
prettyPrintExerciseB (IfStatement condition trueBranch) 
    | totalElements (IfStatement condition trueBranch) > 1 && shouldPrintMultiline (IfStatement condition trueBranch) =
        "if ( " ++ trim (prettyPrintExerciseA condition) ++ " ) " ++ prettyPrintBlock trueBranch
    | otherwise = "if ( " ++ trim (prettyPrintExerciseA condition) ++ " ) " ++ prettyPrintBlock trueBranch
prettyPrintExerciseB (IfElseStatement condition trueBranch falseBranch) 
    | totalElements (IfStatement condition trueBranch) > 1 && shouldPrintMultiline (IfElseStatement condition trueBranch falseBranch) =
        "if ( " ++ trim (prettyPrintExerciseA condition) ++ " ) " ++ prettyPrintExerciseC trueBranch ++ " else " ++ prettyPrintBlock falseBranch
    | otherwise =
        "if ( " ++ trim (prettyPrintExerciseA condition) ++ " ) " ++ prettyPrintBlock trueBranch ++ 
        " else " ++ prettyPrintBlock falseBranch
prettyPrintExerciseB (ReturnStatement expr) = "return " ++ trim (prettyPrintExerciseC expr) ++ ";"
prettyPrintExerciseB (ConstAndBlock decls stmt) = prettyPrintConstsAndStatement (ConstAndBlock decls stmt)
prettyPrintExerciseB adt = prettyPrintExerciseA adt



--------------------------------------------------------------------------------------------------------------------------------------------------

-- | Exercise C


parseConst :: Parser ADT
parseConst = do
    _ <- optional spaces
    _ <- opStr "const" <* spaces
    declarations <- sepBy1 parseConstDeclaration (spaces *> semicolon <* spaces)
    pure $ ConstMultiDeclarations declarations

parseParenExpr :: Parser ADT
parseParenExpr = surroundedBy openParen (parseEmbeddedFunction <|> parseFullExpression) closeParen

parseBinaryOp :: Parser ADT
parseBinaryOp = do
    left <- parseEmbeddedFunction <|> parseFullExpression
    op <- spaces *> string "+" <* spaces
    right <- parseEmbeddedFunction <|> parseFullExpression
    pure $ BinaryOp op left right

parseBinaryOpParen :: Parser ADT
parseBinaryOpParen = surroundedBy openParen parseBinaryOp closeParen


parseArgument :: Parser ADT
parseArgument = spaces *> ( parseExpr <|> parseEmbeddedFunction <|> parseFunctionCall <|> parseBinaryOpParen <|> parseBinaryOp) <* spaces

parseFunctionCall :: Parser ADT
parseFunctionCall = do
    name <- identifier <* spaces
    params <- surroundedBy openParen (parseExpr `sepBy` commaTok) closeParen
    semcol <- parseSemicolon
    pure $ FunctionCall name params

parseEmbeddedFunction :: Parser ADT
parseEmbeddedFunction = do
    name <- identifier <* spaces
    params <- surroundedBy openParen (parseExpr `sepBy` commaTok) closeParen
    -- semcol <- optional parseSemicolon
    pure $ EmbeddedFunction name params

parseFullExpression :: Parser ADT
parseFullExpression = 
         parseFunctionStructure
        <|> parseBinaryOpParen
        <|> parseParenExpr
        <|> parseTernary 
        <|> parseIf
        <|> parseOr 
        <|> parseAnd 
        <|> parseNot
        <|> parseComparison 
        <|> parseFunctionCall
        <|> parseArithmetic 
        <|> parseEmbeddedFunction
        <|> parseConst
        <|> parseElement
        
        


-- Exercise 2

parseFunctionStructure :: Parser ADT
parseFunctionStructure = do
    _ <- spaces *> string "function" <* spaces
    name <- identifier
    _ <- spaces *> openParen <* spaces
    params <- sepBy identifier (op ',')
    _ <- closeParen <* spaces
    body <- parseBlock
    pure $ FunctionStructure name params [body]

parameters :: Parser [String]
parameters = sepBy identifier (op ',')


parseReturn :: Parser ADT
parseReturn = do
    _ <- spaces *> string "return"
    expr <- spaces *> (parseEmbeddedFunction <|> parseFullExpression) <* spaces
    _ <- optional parseSemicolon
    pure $ ReturnStatement expr



isMultiLine :: ADT -> Bool
isMultiLine (FunctionCall _ _) = True
isMultiLine (FunctionStructure _ _ _) = True
isMultiLine (BinaryOp _ _ _) = False 
isMultiLine _ = False


printFunctionCall :: ADT -> String
printFunctionCall (FunctionCall name params) = 
    name ++ "(" ++ intercalate ", " (map printFunctionCall params) ++ ")"
printFunctionCall other = prettyPrintExerciseC other


addSemicolonIfNeeded :: String -> String
addSemicolonIfNeeded str = str ++ if last str == ';' then "" else ";"




isRecursiveCall :: String -> Int -> ADT -> Bool
isRecursiveCall fname expectedParamLength (ReturnStatement (EmbeddedFunction f params))
    | fname == f && length params == expectedParamLength = True
isRecursiveCall fname expectedParamLength (IfStatement _ (Program adts)) = 
    any (isRecursiveCall fname expectedParamLength) adts
isRecursiveCall fname expectedParamLength (IfElseStatement _ (Program trueAdts) (Program falseAdts)) = 
    any (isRecursiveCall fname expectedParamLength) trueAdts || any (isRecursiveCall fname expectedParamLength) falseAdts
isRecursiveCall _ _ _ = False



-- Helper function to filter out only return statements
isReturn :: ADT -> Bool
isReturn (ReturnStatement _) = True
isReturn _ = False


noRecursiveCallsExceptLast :: String -> [ADT] -> Bool
noRecursiveCallsExceptLast fname stmts = 
    all (not . containsRecursiveCall fname) (init stmts)


-- checkTailRecursion :: String -> [ADT] -> Bool
-- checkTailRecursion fname stmts = 
--     let returnStatements = filter isReturn stmts
--         allButLast = init returnStatements
--         lastStmt = last returnStatements
--         expectedParamLength = length returnStatements  -- assuming this is the number of parameters in the function definition
--     in all (not . (isRecursiveCall fname (-1))) allButLast && isRecursiveCall fname expectedParamLength lastStmt



containsRecursiveCall :: String -> ADT -> Bool
containsRecursiveCall fname (EmbeddedFunction f _) = fname == f
containsRecursiveCall fname (Block stmts) = any (containsRecursiveCall fname) stmts
containsRecursiveCall _ _ = False


containsAnyFunctionCallExcept :: String -> ADT -> Bool
containsAnyFunctionCallExcept fname (FunctionCall f _) = fname /= f
containsAnyFunctionCallExcept fname (EmbeddedFunction f _) = fname /= f
containsAnyFunctionCallExcept fname (Block stmts) = any (containsAnyFunctionCallExcept fname) stmts
containsAnyFunctionCallExcept _ _ = False


isTailRecursive :: String -> Bool
isTailRecursive str = 
    case parse parseExerciseC str of
        Result _ (Program [FunctionStructure name params [Block body]]) -> isTailRec name params body
        _ -> False
  where
    isTailRec name params body
        | null body = False
        | otherwise =
            let noRec = noRecursiveCallsExceptLast name body
                expectedParamLength = length params
                tailRec = isRecursiveCall name expectedParamLength (last body)
            in noRec && tailRec


lastAndInit :: [a] -> Maybe (a, [a])
lastAndInit [] = Nothing
lastAndInit xs = Just (last xs, init xs)



flattenStatements :: [ADT] -> [ADT]
flattenStatements [] = []
flattenStatements ((Block stmts):rest) = flattenStatements stmts ++ flattenStatements rest
flattenStatements ((Program stmts):rest) = flattenStatements stmts ++ flattenStatements rest
flattenStatements ((IfStatement cond (Program progStmts)):rest) = 
    IfStatement cond (Program (flattenStatements progStmts)) : flattenStatements rest
flattenStatements (stmt:rest) = stmt : flattenStatements rest





transformToWhileLoop :: ADT -> ADT
transformToWhileLoop func@(FunctionStructure fname params body)
    | isTailRecursive (adtToString func) =
        FunctionStructure fname params [WhileLoop (BoolLit True) (transformBody params body)]
    | otherwise = func

isIfStatement :: ADT -> Bool
isIfStatement (IfStatement _ _) = True
isIfStatement _ = False

transformBody :: [String] -> [ADT] -> [ADT]
transformBody params body = 
    trace ("Transforming body for params: " ++ show params ++ " and body: " ++ show body) $
    let flattenedBody = flattenStatements body
    in trace ("Flattened body: " ++ show flattenedBody) $ 
       if isTailRecReturn (last flattenedBody) then
           replaceTailRecWithDestructure flattenedBody
       else
           map (transformNonTailRecursiveReturn params) flattenedBody
  where
    replaceTailRecWithDestructure :: [ADT] -> [ADT]
    replaceTailRecWithDestructure stmts 
        | null stmts = []
        | isTailRecReturn (last stmts) = 
            init stmts ++ [DestructureRecursion params (extractParamsFromReturn $ last stmts)]
        | otherwise = stmts  -- This change ensures that all other statements remain as they are.
    
    isTailRecReturn :: ADT -> Bool
    isTailRecReturn (ReturnStatement (EmbeddedFunction _ _)) = 
        trace "Tail recursive return detected!" True
    isTailRecReturn (Block stmts) =
        trace "Checking inside block..." $
        if null stmts then False else isTailRecReturn (last stmts)
    isTailRecReturn _ = 
        trace "No tail recursive return detected!" False

    extractParamsFromReturn :: ADT -> [ADT]
    extractParamsFromReturn (ReturnStatement (EmbeddedFunction _ args)) = args
    extractParamsFromReturn (Block stmts) = 
        trace "Extracting parameters from block..." $
        if null stmts then [] else extractParamsFromReturn (last stmts)
    extractParamsFromReturn _ = []

    transformNonTailRecursiveReturn :: [String] -> ADT -> ADT
    transformNonTailRecursiveReturn params (Block stmts) = Block (map (transformNonTailRecursiveReturn params) stmts)
    transformNonTailRecursiveReturn params stmt = stmt


    -- transformTailRecursive _ other = [other]



adtToString :: ADT -> String
adtToString (WhileLoop cond body) = 
    "while (" ++ prettyPrintExerciseC cond ++ ") {\n" 
    ++ concatMap (\stmt -> "    " ++ adtToString stmt ++ "\n") body
    ++ "}"
adtToString (FunctionStructure name params body) =
    "function " ++ name ++ "(" ++ intercalate ", " params ++ ") " 
        ++ intercalate "\n" (map prettyPrintExerciseC body)
    
adtToString _ = ""


parseExerciseC :: Parser ADT
parseExerciseC =      
             parseReturn
             <|> parseAllStatements
             <|> parseFullExpression  
             <|> parseExpr
             <|> parseBlock
             <|> parseArgument 



prettyPrintStatementWithoutBraces :: ADT -> String
prettyPrintStatementWithoutBraces (Block stmts) = "{\n" ++ (indent $ concatMap (\stmt -> prettyPrintStatementWithoutBraces stmt ++ "\n") stmts) ++ "}"
prettyPrintStatementWithoutBraces (DestructureRecursion vars vals) =
    "[" ++ intercalate ", " vars ++ "] = [" ++ intercalate ", " (map prettyPrintStatementWithoutBraces vals) ++ "];"
prettyPrintStatementWithoutBraces (IfStatement cond trueBranch) = 
    "if ( " ++ prettyPrintExerciseC cond ++ " )" 
    ++ indent (prettyPrintExerciseC trueBranch) 
prettyPrintStatementWithoutBraces otherStmt = trim (prettyPrintExerciseC otherStmt)



        
prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC (Program adts) = intercalate "\n" $ map prettyPrintExerciseC adts
prettyPrintExerciseC (FunctionCall name params) = 
    name ++ "(" ++ intercalate ", " (map prettyPrintExerciseC params) ++ ");"
prettyPrintExerciseC (EmbeddedFunction name params) = 
    name ++ "(" ++ intercalate ", " (map prettyPrintExerciseC params) ++ ")"
    
prettyPrintExerciseC func@(FunctionStructure name params body)  
    | isTailRecursive (adtToString func) = 
        "function " ++ name ++ "(" ++ intercalate ", " params ++ ") { \n" ++ 
        "    while (true) {\n" ++
        indentForWhile (concatMap (\stmt -> prettyPrintStatementWithoutBraces stmt ++ "\n") (transformBody params body)) ++
        "    }\n" ++
        "}"
    | otherwise = 
        "function " ++ name ++ "(" ++ intercalate ", " params ++ ") " 
        ++ intercalate "\n" (map prettyPrintExerciseC body)

prettyPrintExerciseC (ReturnStatement expr) =  "return " ++ trim(prettyPrintExerciseC expr) ++ ";"

prettyPrintExerciseC block@(Block _) = prettyPrintBlock block
prettyPrintExerciseC (Var varName) = varName
prettyPrintExerciseC (ConstDeclaration varName expr) = "const " ++ varName ++ " = " ++ printFunctionCall expr ++ ";"
prettyPrintExerciseC (ConstMultiDeclarations adts) = 
    intercalate "\n" $ map prettyPrintExerciseC adts
prettyPrintExerciseC (IfStatement cond trueBranch) = 
    prettyPrintStatementWithoutBraces (IfStatement cond trueBranch)
prettyPrintExerciseC (IfElseStatement cond trueBranch falseBranch) = 
    "if ( " ++ prettyPrintExerciseC cond ++ " ) " ++ wrapWithBraces (prettyPrintExerciseC trueBranch) ++ 
    " else " ++ prettyPrintExerciseC falseBranch
prettyPrintExerciseC (StringLit s) = "\"" ++ s ++ "\""
prettyPrintExerciseC (BinaryOp op a b) = 
    let leftStr = case a of
                     BinaryOp _ _ _ -> "(" ++ prettyPrintExerciseC a ++ ")"
                     _              -> prettyPrintExerciseC a
        rightStr = case b of
                      BinaryOp _ _ _ -> "(" ++ prettyPrintExerciseC b ++ ")"
                      _              -> prettyPrintExerciseC b
    in leftStr ++ " " ++ op ++ " " ++ rightStr

prettyPrintExerciseC (IntLit i) = show i
prettyPrintExerciseC (List items) = "[" ++ intercalate ", " (map prettyPrintExerciseC items) ++ "]"
prettyPrintExerciseC (Identifier name) = name
prettyPrintExerciseC (BoolLit b) = if b then "true" else "false"
prettyPrintExerciseC (Add e1 e2) = "(" ++ prettyPrintExerciseC e1 ++ " + " ++ prettyPrintExerciseC e2 ++ ")"
prettyPrintExerciseC (ConstAndBlock decls stmt) = prettyPrintConstsAndStatement (ConstAndBlock decls stmt)
prettyPrintExerciseC adt = prettyPrintExerciseA adt



-- prettyPrintWhileLoop :: ADT -> String
-- prettyPrintWhileLoop (FunctionStructure name params body) =
--     case lastAndInit body of
--         Just (DestructureRecursion vars vals, restOfBody) ->
--             "function " ++ name ++ "(" ++ intercalate ", " params ++ ") {\n" ++
--             indent (concatMap (\stmt -> prettyPrintExerciseC stmt ++ "\n") restOfBody) ++
--             "        [" ++ intercalate ", " vars ++ "] = [" ++ intercalate ", " (map prettyPrintExerciseC vals) ++ "];\n" ++
--             "    }\n"
--         -- _ -> "function " ++ name ++ "(" ++ intercalate ", " params ++ ") { ... }"  -- Some default behavior

