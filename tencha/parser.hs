module Parser where 

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


-- Defining Lexer using combinator library in Haskell
languageDef =
    emptyDef {
        Token.commentStart = "/*",
        Token.commentEnd = "*/",
        Token.commentLine = "//",
        Token.identStart = letter,
        Token.identLetter = alphaNum,
        Token.reservedNames = ["if", "then", "true", "false",
                                "skip", "assume", "assert", "havoc",
                                "while", "program", "requires", "ensures"],
        Token.reservedOpNames = ["+", "-", "*", "/", ":=", "<", 
                                ">", "<=", ">=", "!", "&&", "||", "=>", "<=>"]
                        
    }

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved = Token.reserved lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whitespace = Token.whiteSpace lexer 

-- parsing trailing white space
-- whileParser :: Parser Com
-- whileParser = whitespace >> com

{-
    Defining expression data types
-}
data Expr = 
          AConst Int
        | BConst Bool
        | Var String
        | BinOp Bop Expr Expr 
        | UnOp Uop Expr
    deriving(Eq)

data Bop = Plus | Minus | Times
        | Div | Eq | Ne
        | Lt | Gt | Ge
        | And | Or | Impl
        | Iff
    deriving (Eq)

data Uop = UNot | UMinus
    deriving (Eq)

plus, minus, times, div, eq, ne, lt :: Expr -> Expr -> Expr
plus = BinOp Plus -- this is valid because Op Plus is type Expr, which has a function, that takse an Expr and return Expr -> Expr for us
minus = BinOp Minus
times = BinOp Times
div = BinOp Div
eq = BinOp Eq
ne = BinOp Ne
lt = BinOp Lt
gt = BinOp Gt
ge = BinOp Ge
and = BinOp And
or = BinOp Or
impl = BinOp Impl
iff = BinOp Iff
unot = UnOp UNot
uminus = UnOp UMinus

example :: Expr
example = times (AConst 3) (AConst 5)

data Com =  Skip 
        | Assign String Expr
        | Havoc String
        | Assume Expr
        | Assert Expr
        | Choice Com Com
        | Seq Com Com
        | If Expr Com Com
        | While Expr Expr Com
        deriving(Eq)

data Spec = Requires Expr
            | Ensure Expr
        deriving(Eq)

data Program = Program Spec Spec Com
        deriving(Eq)

com :: Com
com = Assign "x" example

pre :: Expr
pre = ge (Var ("x")) (AConst 0)

spec :: Spec
spec = (Requires pre)

program :: Program
program = Program spec spec (Havoc ("x"))


-- Pretty printer
instance Show Bop where 
    show operation = show (pprBinOp operation)

instance Show Uop where
    show operation = show (pprUop operation)

instance Show Expr where
    show expr = show (pprExpr expr)

instance Show Spec where 
    show spec = show (pprSpec spec)

instance Show Com where
    show com = show (pprCom com)

instance Show Program where
    show program = show (pprProg program)
    

pprUop :: Uop -> String
pprUop UNot = "!"
pprUop UMinus = "-"

pprBinOp :: Bop -> String
pprBinOp Plus = "+"
pprBinOp Minus = "-"
pprBinOp Times = "*"
pprBinOp Div = "/"
pprBinOp Eq = "=="
pprBinOp Ne = "!="
pprBinOp Lt = "<"
pprBinOp Gt = ">"
pprBinOp Ge = ">="
pprBinOp And = "&&"
pprBinOp Or = "|"
pprBinOp Impl = "=>"
pprBinOp Iff = "<=>"

--ask edward
pprExpr :: Expr -> String
pprExpr = go (0 :: Int)
    where go n e = paren n (go' e)
          go' (BConst (bool))  = (11, show bool)
          go' (AConst (value)) = (10, show value) 
          go' (Var (name))      = (9, name)
          go' (BinOp operation e1 e2)  = (6, go 5 e1 ++ " " ++ pprBinOp operation ++ " " ++ go 6 e2)
          go' (UnOp operation e) = (3, pprUop operation ++ go 2 e)
          paren n (m, s) | m > n     = s
                         | otherwise = "(" ++ s ++ ")"

pprSpec :: Spec -> String
pprSpec (Requires expr) = "assume " ++ pprExpr (expr)
pprSpec (Ensure expr) = "assert " ++ pprExpr (expr)

--finishing other cases
pprCom :: Com -> String
pprCom (Skip) = ""
pprCom (Assign name expr) = name ++ " := " ++ pprExpr expr ++ ";"
pprCom (Havoc name) = "havoc " ++ name
pprCom (Assume expr) = "assume " ++ pprExpr expr
pprCom (Assert expr) = "assert " ++ pprExpr expr

pprProg :: Program -> String
pprProg (Program spec1 spec2 com) = 
    "program" ++ pprSpec spec1 ++ "\n" ++ pprSpec spec2 ++ 
    "\n" ++ pprCom com

-- type checker -> Very loosly defined

data Type = TInt | TBool | TUnit

