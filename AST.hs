module AST where

{- Grupo 1
   Árvore -}

data SL = Program String [DeclProg] Boolean [Decl] [Inst] Boolean Boolean
        deriving Show

data Decl = Atr  TypeDecl String Expr
          | AtrT TypeDecl String
          | AtrS TypeDecl [Expr]
          deriving (Show , Eq , Ord)

data DeclProg = AtrProg TypeDecl String
          deriving (Show , Eq , Ord)

data TypeDecl = Int | Char | Bool
              deriving (Show , Eq , Ord)

data Inst = Assign      String Expr
          | IfThenElse  Boolean [Inst] [Inst]
          | For         Decl Boolean Expr [Inv] [Inst]
          | While       Boolean Boolean [Inst]
          | Read        Expr
          | Print       Expr
          | Return      Expr
          | Try         [Inst] [Inst]
          | Throw
          deriving (Show , Eq , Ord)

data Inv = Inv Boolean
         deriving (Show, Eq, Ord)

data Expr = Const Integer
          | Var   String
          | Add   Expr Expr
          | Mul   Expr Expr
          | Div   Expr Expr
          | Sub   Expr Expr
          | Same  Expr Expr
          deriving (Show , Eq , Ord)

data Boolean = Expr         Expr
             | Greater      Expr Expr
             | GreaterEqual Expr Expr
             | Less         Expr Expr
             | LessEqual    Expr Expr
             | And          Expr Boolean
             | Orl          Expr Boolean
             | Equal        Expr Expr
             | Different    Expr Expr
             | BoolConst    Bool
             | Implies      Expr Boolean
             | Not          Expr
             deriving (Show , Eq , Ord)

data Boolean2 = Expr2        Expr
             | Greater2      Expr Expr
             | GreaterEqual2 Expr Expr
             | Less2         Expr Expr
             | LessEqual2    Expr Expr
             | And2          Boolean2 Boolean2
             | Orl2          Boolean2 Boolean2
             | Equal2        Expr Expr
             | Different2    Expr Expr
             | BoolConst2    Bool
             | Implies2      Boolean2 Boolean2
             | Not2          Boolean2
             deriving (Show , Eq , Ord)
