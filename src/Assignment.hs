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
         | ConstDeclaration String ADT --B
         | ConstMultiDeclarations [ADT] --B
         | IfStatement ADT ADT --B
         | IfElseStatement ADT ADT ADT -- condition, if-body, else-body
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

openCurly :: Parser Char
openCurly = op '{'

closeCurly :: Parser Char
closeCurly = op '}' 

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

surroundedBy :: Parser a -> Parser b -> Parser a -> Parser b
surroundedBy open p close = do
    _ <- open
    x <- p
    _ <- close
    return x

parseExpr :: Parser ADT
parseExpr =
        parseFunctionStructure
        <|> parseBinaryOp
        <|> parseTernary 
        <|> parseOr 
        <|> parseAnd 
        <|> parseNot
        <|> parseComparison 
        <|> parseEmbeddedFunction
        <|> parseFunctionCall
        <|> parseArithmetic 
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
    multiline("if ( " ++ prettyPrintExerciseC cond ++ " ) " ++ prettyPrintExerciseC trueBranch)
    || anyChildIsMultiline block
shouldPrintMultiline block@(IfElseStatement cond trueBranch falseBranch) = 
    multiline("if ( " ++ trim (prettyPrintExerciseA cond) ++ " ) " ++ prettyPrintBlock trueBranch ++ " else " ++ wrapWithBraces(prettyPrintExerciseBForBlock falseBranch))
    || anyChildIsMultiline block
shouldPrintMultiline _ = False


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

-- Exercise 2

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
prettyPrintBlock (IfStatement condition trueBranch)
    | shouldPrintMultiline (IfStatement condition trueBranch) = 
        "if ( " ++ prettyPrintExerciseB condition ++ " )" ++
        (prettyPrintExerciseB trueBranch)
    | otherwise = 
        "if ( " ++ trim (prettyPrintExerciseB condition) ++ " ) " ++ 
        wrapWithBraces(prettyPrintExerciseBForBlock trueBranch)
prettyPrintBlock stmt@(IfElseStatement condition trueBranch falseBranch) 
    | shouldPrintMultiline stmt =
        "if ( " ++ trim (prettyPrintExerciseA condition) ++ " ) {\n" ++ indent (prettyPrintExerciseBForBlock trueBranch) ++ "\n} else {\n" ++ indent (prettyPrintExerciseBForBlock falseBranch) ++ "\n}"
    | otherwise = 
        "if ( " ++ trim (prettyPrintExerciseA condition) ++ " ) " ++ prettyPrintExerciseBForBlock trueBranch ++ " else " ++ prettyPrintExerciseBForBlock falseBranch
prettyPrintBlock (Block []) = "{ }"
prettyPrintBlock block@(Block [innerBlock@(Block _)]) = prettyPrintBlock innerBlock
prettyPrintBlock block@(Block [stmt@(IfStatement _ _)]) =
    prettyPrintExerciseB stmt
prettyPrintBlock block@(Block stmts)
    | not (shouldPrintMultilineIfElse block) && totalElements block ==1 =
        "{ \n" ++ indent (intercalate "\n" $ map (trim . prettyPrintExerciseBForBlock) stmts) ++ "}"
    | otherwise = wrapWithBraces (intercalate "\n" $ map (trim . prettyPrintExerciseBForBlock) stmts)
prettyPrintBlock (Identifier varName) = varName
prettyPrintBlock (ReturnStatement expr) = "return " ++ prettyPrintExerciseC expr ++ ";"
prettyPrintBlock (ConstDeclaration varName expr) = "const " ++ varName ++ " = " ++ trim (prettyPrintExerciseA expr) ++ ";"
prettyPrintBlock (ConstMultiDeclarations adts) = intercalate ";\n" $ map prettyPrintConstDeclaration adts
prettyPrintBlock (Program adts) = intercalate "\n" $ map prettyPrintExerciseB adts


prettyPrintExerciseBForBlock :: ADT -> String
prettyPrintExerciseBForBlock (ConstMultiDeclarations adts) = 
    intercalate "\n" $ map prettyPrintConstDeclaration adts
prettyPrintExerciseBForBlock (Block []) = ""
prettyPrintExerciseBForBlock block@(Block stmts) 
    | not (shouldPrintMultiline block) && totalElements block == 1 = 
        intercalate "\n" $ map (trim . prettyPrintExerciseBForBlock) stmts
    | otherwise = wrapWithBraces (intercalate "\n" $ map (trim . prettyPrintExerciseBForBlock) stmts)
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
                       <|> parseIdentifier

parseStatement :: Parser ADT
parseStatement = parseReturn
            <|> parseIf
            <|> parseConstsThenIf
            <|> parseConstAndBlock
            <|> parseMultipleConstDeclarations 
            <|> parseConstDeclaration
            <|> parseFunctionCall
            <|> parseEmbeddedFunction
            <|> parseIdentifier 
            <|> parseFullExpression

parseAllStatements :: Parser ADT
parseAllStatements = do
    statements <- many1 (parseStatement <|> parseBlock)
    return $ Program statements

parseExerciseB :: Parser ADT
parseExerciseB = parseAllStatements

prettyPrintConstsAndStatement :: ADT -> String
prettyPrintConstsAndStatement (ConstAndBlock decls stmt) =
    intercalate "\n" (map prettyPrintConstDeclaration decls) ++ "\n" ++ prettyPrintExerciseB stmt


prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (Identifier varName) = varName
prettyPrintExerciseB block@(Block _) = prettyPrintBlock block
prettyPrintExerciseB (Program adts) = intercalate "\n" $ map prettyPrintExerciseB adts
prettyPrintExerciseB (ConstDeclaration varName expr) = "const " ++ varName ++ " = " ++ trim (prettyPrintExerciseA expr) ++ ";"
prettyPrintExerciseB (ConstMultiDeclarations adts) = 
    intercalate ";\n" $ map prettyPrintConstDeclaration adts
prettyPrintExerciseB (IfStatement condition trueBranch)
    | shouldPrintMultiline (IfStatement condition trueBranch) = 
        "if ( " ++ prettyPrintExerciseC condition ++ " )" ++
        (prettyPrintExerciseC trueBranch)
    | otherwise = 
        "if ( " ++ trim (prettyPrintExerciseB condition) ++ " ) " ++ 
        wrapWithBraces(prettyPrintExerciseBForBlock trueBranch)
prettyPrintExerciseB (IfElseStatement condition trueBranch falseBranch) 
    | shouldPrintMultiline (IfElseStatement condition trueBranch falseBranch)  =
        "if ( " ++ trim (prettyPrintExerciseB condition) ++ " ) {\n" ++ indent(prettyPrintExerciseBForBlock trueBranch) ++ 
        "} else {\n" ++ indent (prettyPrintExerciseBForBlock falseBranch) ++ "}"
    | otherwise = 
        "if ( " ++ trim (prettyPrintExerciseB condition) ++ " ) " ++ prettyPrintBlock trueBranch ++ 
        " else " ++ wrapWithBraces(prettyPrintExerciseBForBlock falseBranch)
prettyPrintExerciseB (ReturnStatement expr) = "return " ++ trim (prettyPrintExerciseC expr) ++ ";"
prettyPrintExerciseB (ConstAndBlock decls stmt) = prettyPrintConstsAndStatement (ConstAndBlock decls stmt)
prettyPrintExerciseB adt = prettyPrintExerciseA adt


--------------------------------------------------------------------------------------------------------------------------------------------------

-- | Exercise C

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
    pure $ EmbeddedFunction name params


parseFullExpression :: Parser ADT
parseFullExpression = 
         parseFunctionStructure
        <|> parseBinaryOpParen
        <|> parseParenExpr
        <|> parseIf
        <|> parseFunctionCall
        <|> parseArithmetic 
        <|> parseEmbeddedFunction
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

parseReturn :: Parser ADT
parseReturn = do
    _ <- spaces *> string "return"
    expr <- spaces *> (parseFunctionCall <|> parseExpr) <* spaces
    _ <- optional parseSemicolon
    pure $ ReturnStatement expr


printFunctionCall :: ADT -> String
printFunctionCall (FunctionCall name params) = 
    name ++ "(" ++ intercalate ", " (map printFunctionCall params) ++ ")"
printFunctionCall other = prettyPrintExerciseC other


isRecursiveCall :: String -> Int -> ADT -> Bool
isRecursiveCall fname expectedParamLength (ReturnStatement (EmbeddedFunction f params))
    | fname == f && length params == expectedParamLength = True
isRecursiveCall fname expectedParamLength (IfStatement _ (Program adts)) = 
    any (isRecursiveCall fname expectedParamLength) adts
isRecursiveCall fname expectedParamLength (IfElseStatement _ (Program trueAdts) (Program falseAdts)) = 
    any (isRecursiveCall fname expectedParamLength) trueAdts || any (isRecursiveCall fname expectedParamLength) falseAdts
isRecursiveCall _ _ _ = False


noRecursiveCallsExceptLast :: String -> [ADT] -> Bool
noRecursiveCallsExceptLast fname stmts 
    | null stmts = True
    | otherwise  = 
        let noRecInInit = not $ any (containsRecursiveCall fname) (init stmts)
            noRecInLast = not $ containsRecursiveCall fname (last stmts)
        in trace ("Checking last statement for recursion and all others for absence of recursion") noRecInInit && noRecInLast

containsEmbeddedFunction :: ADT -> Bool
containsEmbeddedFunction (FunctionCall _ _) = True
containsEmbeddedFunction (Block stmts) = any containsEmbeddedFunction stmts
containsEmbeddedFunction (IfStatement _ body) = containsEmbeddedFunction body
containsEmbeddedFunction (IfElseStatement _ trueBody falseBody) = 
    containsEmbeddedFunction trueBody || containsEmbeddedFunction falseBody
containsEmbeddedFunction (ReturnStatement expr) = 
    case expr of
        FunctionCall _ _ -> True
        _ -> False
containsEmbeddedFunction _ = False

containsRecursiveCall :: String -> ADT -> Bool
containsRecursiveCall fname (FunctionCall f _) = 
    trace ("Checking for recursion: " ++ fname) fname == f
containsRecursiveCall fname (Block stmts) = 
    trace ("Checking Block for recursion") any (containsRecursiveCall fname) stmts
containsRecursiveCall fname (IfStatement _ (Block[ReturnStatement (FunctionCall f _)])) = 
     trace ("----Checking if statement body for recursion") True
containsRecursiveCall fname (IfElseStatement _ trueBody falseBody) = 
    trace ("Checking IfElseStatement for recursion") containsRecursiveCall fname trueBody || containsRecursiveCall fname falseBody
containsRecursiveCall fname (Program adts) = 
    trace ("Checking Program for recursion") any (containsRecursiveCall fname) adts
containsRecursiveCall _ _ = False


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
            in trace ("No recursion in statements except last: " ++ show noRec ++ ". Last statement is tail recursive: " ++ show tailRec) noRec && tailRec


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


isTailRecReturn :: ADT -> Bool
isTailRecReturn (ReturnStatement (FunctionCall _ _)) = True
isTailRecReturn (Block stmts)
    | null stmts = False
    | otherwise = isTailRecReturn (last stmts)
isTailRecReturn _ = False

extractParamsFromReturn :: ADT -> [ADT]
extractParamsFromReturn (ReturnStatement (FunctionCall _ args)) = args
extractParamsFromReturn (Block stmts)
    | null stmts = []
    | otherwise = extractParamsFromReturn (last stmts)
extractParamsFromReturn _ = []

transformBody :: [String] -> [ADT] -> [ADT]
transformBody params body = 
    let flattenedBody = flattenStatements body
    in if isTailRecReturn (last flattenedBody) then
           replaceTailRecWithDestructure flattenedBody
       else
           map (transformNonTailRecursiveReturn params) flattenedBody
  where
    replaceTailRecWithDestructure :: [ADT] -> [ADT]
    replaceTailRecWithDestructure stmts 
        | null stmts = []
        | isTailRecReturn (last stmts) = 
            init stmts ++ [DestructureRecursion params (extractParamsFromReturn $ last stmts)]
        | otherwise = stmts 

    transformNonTailRecursiveReturn :: [String] -> ADT -> ADT
    transformNonTailRecursiveReturn params (Block stmts) = Block (map (transformNonTailRecursiveReturn params) stmts)
    transformNonTailRecursiveReturn params stmt = stmt


adtToString :: ADT -> String
adtToString (WhileLoop cond body) = 
    "while (" ++ prettyPrintExerciseC cond ++ ") {\n" 
    ++ concatMap (\stmt -> "    " ++ adtToString stmt ++ "\n") body
    ++ "}"
adtToString (FunctionStructure name params body) =
    "function " ++ name ++ "(" ++ intercalate ", " params ++ ") " 
        ++ intercalate "\n" (map prettyPrintExerciseC body)
adtToString _ = ""


prettyPrintStatementWithoutBraces :: ADT -> String
prettyPrintStatementWithoutBraces (Block stmts) = "{\n" ++ (indent $ concatMap (\stmt -> prettyPrintStatementWithoutBraces stmt ++ "\n") stmts) ++ "}"
prettyPrintStatementWithoutBraces (DestructureRecursion vars vals) =
    "[" ++ intercalate ", " vars ++ "] = [" ++ intercalate ", " (map prettyPrintStatementWithoutBraces vals) ++ "];"
prettyPrintStatementWithoutBraces (IfStatement cond trueBranch)
    | shouldPrintMultiline (IfStatement cond trueBranch) = 
        "if ( " ++ prettyPrintExerciseC cond ++ " ) " 
        ++  (prettyPrintExerciseC trueBranch)
    | otherwise = 
        "if ( " ++ prettyPrintExerciseC cond ++ " ) " ++ wrapWithBraces(prettyPrintExerciseBForBlock trueBranch) 
prettyPrintStatementWithoutBraces otherStmt = trim (prettyPrintExerciseC otherStmt)


parseExerciseC :: Parser ADT
parseExerciseC = parseAllStatements
             <|> parseReturn
             <|> parseExpr
             <|> parseFullExpression  
             <|> parseBlock
        
prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC (Program adts) = intercalate "\n" $ map prettyPrintExerciseC adts
prettyPrintExerciseC (FunctionCall name params) = 
    name ++ "(" ++ intercalate ", " (map prettyPrintExerciseC params) ++ ");"
prettyPrintExerciseC (EmbeddedFunction name params) = 
    name ++ "(" ++ intercalate ", " (map prettyPrintExerciseC params) ++ ")"
prettyPrintExerciseC func@(FunctionStructure name params body)  
    | isTailRecursive (adtToString func) = 
        "function " ++ name ++ "(" ++ intercalate ", " params ++ ") { \n" ++ 
        "    while (true) { \n" ++
        indentForWhile (concatMap (\stmt -> prettyPrintStatementWithoutBraces stmt ++ "\n") (transformBody params body)) ++
        "    }\n" ++
        "}"
    | otherwise = 
        "function " ++ name ++ "(" ++ intercalate ", " params ++ ") " 
        ++ intercalate "\n" (map prettyPrintExerciseC body)
prettyPrintExerciseC (ReturnStatement expr) =  "return " ++ trim(prettyPrintExerciseC expr) ++ ";"
prettyPrintExerciseC (Identifier varName) = varName
prettyPrintExerciseC (ConstDeclaration varName expr) = "const " ++ varName ++ " = " ++ printFunctionCall expr ++ ";"
prettyPrintExerciseC (ConstMultiDeclarations adts) = 
    intercalate "\n" $ map prettyPrintExerciseC adts
prettyPrintExerciseC stmt@(IfStatement cond trueBranch)
    | shouldPrintMultiline stmt = 
        "if ( " ++ prettyPrintExerciseC cond ++ " ) {\n" ++
        indent (prettyPrintExerciseC trueBranch) ++ "\n}"
    | otherwise = 
        "if ( " ++ prettyPrintExerciseC cond ++ " ) " ++ 
        wrapWithBraces(prettyPrintExerciseBForBlock trueBranch)
prettyPrintExerciseC stmt@(IfElseStatement cond trueBranch falseBranch)
    | shouldPrintMultiline stmt =
        "if ( " ++ trim (prettyPrintExerciseB cond) ++ " ) {\n" ++ indent (prettyPrintExerciseBForBlock trueBranch) ++ 
        "\n} else {\n" ++ indent (prettyPrintExerciseBForBlock falseBranch) ++ "\n}"
    | otherwise = 
        "if ( " ++ trim (prettyPrintExerciseA cond) ++ " ) " ++ prettyPrintExerciseBForBlock trueBranch ++ 
        " else " ++ prettyPrintExerciseBForBlock falseBranch
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
prettyPrintExerciseC block@(Block _) = prettyPrintBlock block
prettyPrintExerciseC adt = prettyPrintExerciseA adt

