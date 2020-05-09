module Parser where 

{-
    Defining expression data types
-}

data Expr = 
          AConst Int
        | BConst Bool
        | Var String
        | BinOp Bop Expr Expr 
        | UnOp Uop Expr
    deriving(Eq, Show)

data Bop = Plus | Minus | Times
        | Div | Eq | Ne
        | Lt | Gt | Ge
        | And | Or | Impl
        | Iff
    deriving (Show, Eq)

data Uop = UNot | UMinus
    deriving (Show, Eq)

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
example = times (plus (AConst 1) (AConst 2)) (AConst 3)

data Com =  Skip 
        | Assign String Expr
        | Havoc String
        | Assume Expr
        | Assert Expr
        | Choice Com Com
        | Seq Com Com
        | If Expr Com Com
        | While Expr Expr Com
        deriving(Show, Eq)

data Spec = Requires Expr
            | Ensure Expr
        deriving(Show, Eq)

data Program = Program Spec Com
        deriving(Show, Eq)

com :: Com
com = Assign "x" example

pre :: Expr
pre = ge (Var ("x")) (AConst 0)

spec :: Spec
spec = (Requires pre)

program :: Program
program = Program spec (Havoc ("x"))


-- Pretty printer