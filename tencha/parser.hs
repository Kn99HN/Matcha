module Parser where 

{-
    Defining expression data types
-}

data Expr = Const 
        | BinOp Bop Expr Expr 
        | UnOp Uop Expr
    deriving(Eq)

data Const = AConst Int 
            | BConst Bool 
            | Var String
            deriving (Eq, Show)

data Bop = Plus | Minus | Times
        | Div | Eq | Ne
        | Lt | Gt | Ge
        | And | Or | Impl
        | Iff
    deriving (Show, Eq)

data Uop = UNot | UMinus
    deriving (Show, Eq)

-- instance Show Not where 
--     show Not = 
-- main = putStrLn (Not)