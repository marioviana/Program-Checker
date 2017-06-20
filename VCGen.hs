module VCGen where

import ParserSL
import Z3.Monad

intExprToZ3 (Var s) = mkFreshIntVar s

intExprToZ3 (Const c) = mkInteger c

intExprToZ3 (Add a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkAdd [c, d]
}

intExprToZ3 (Mul a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkMul [c, d]
}

intExprToZ3 (Div a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkDiv c d
}

intExprToZ3 (Sub a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkSub [c, d]
}

boolExprToZ3 (BoolConst2 e)
      | e = mkTrue
      | otherwise = mkFalse

boolExprToZ3 (Less2 a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkLt c d
}

boolExprToZ3 (Not2 a) = boolExprToZ3 a >>= mkNot


boolExprToZ3 (LessEqual2 a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkLe c d
}

boolExprToZ3 (Greater2 a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkGt c d
}

boolExprToZ3 (GreaterEqual2 a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkGe c d
}

boolExprToZ3 (Equal2 a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkEq c d
}

boolExprToZ3 (Different2 a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkEq c d >>= mkNot
}



boolExprToZ3 (And2 a b) = do {
  c <- boolExprToZ3 a;
  d <- boolExprToZ3 b;
  mkAnd [c,d]
}

boolExprToZ3 (Orl2 a b) = do {
  c <- boolExprToZ3 a;
  d <- boolExprToZ3 b;
  mkOr [c,d]
}

boolExprToZ3 (Implies2 a b) = do {
  c <- boolExprToZ3 a;
  d <- boolExprToZ3 b;
  mkImplies c d
}



{-vcg :: SL -> [Boolean]
vcg (TinyL p s q1 q2) = [Implies p (wp s q1 q2)] ++ (vcaux s q1 q2)
-}

{-= Assign      String Expr
          | IfThenElse  Boolean [Inst] [Inst]
          | For         Decl Boolean Expr [Inv] [Inst]
          | While       Boolean [Inv] [Inst]
          | Read        Expr
          | Print       Expr
          | Return      Expr
          | Try         [Inst] [Inst]
          | Throw
-}



---- 


vcs :: MonadZ3 z3 => SL -> z3 [AST]
vcs = sequence . vcg2Z3 . vcg

vcs' :: MonadZ3 z3 => SL -> z3 AST
vcs' t = vcs t >>= mkAnd

vcg2Z3 ::MonadZ3 z3 => [Boolean2] -> [z3 AST]
vcg2Z3 s | s == [] = []
         | otherwise = map boolExprToZ3 s


vcg :: SL -> [Boolean2]
vcg (Program a b c d e f g) = [(Implies2 (boolTobool2 c) (wp e (boolTobool2 f) (boolTobool2 g) ))] ++ (vcaux e (boolTobool2 f) (boolTobool2 g))



---------



vcaux :: [Inst] -> Boolean2 -> Boolean2 -> [Boolean2]
vcaux [] q1 q2 = []
vcaux [Throw] q1 q2 = []
vcaux [Assign v i] q1 q2 = []
vcaux [Try s1 s2] q1 q2 = (vcaux s1 q1 q2) ++ (vcaux s2 q1 q2)
vcaux [IfThenElse b s1 s2] q1 q2 = (vcaux s1 q1 q2) ++ (vcaux s2 q1 q2)
vcaux [While b i s] q1 q2 = (([Implies2 (And2 (boolTobool2 i) (boolTobool2 b)) (wp s (boolTobool2 i) q2),
                            Implies2 (And2 (boolTobool2 i) (Not2 (boolTobool2 b))) q1])) ++ (vcaux s (boolTobool2 i) q2)
vcaux (s1:sn) q1 q2 = (vcaux [s1] (wp sn q1 q2) q2) ++ (vcaux sn q1 q2)


wp:: [Inst] -> Boolean2 -> Boolean2 -> Boolean2
wp [] q1 q2 = q1
wp [Throw] q1 q2 = q2
wp [Assign x e] q1 q2 = subBexp q1 x e
wp [Try s1 s2] q1 q2 = (wp s1 q1 (wp s2 q1 q2)) 
wp [IfThenElse b s1 s2] q1 q2 = And2 (Implies2 (boolTobool2 b) (wp s1 q1 q2)) (Implies2 (Not2 (boolTobool2 b)) (wp s2 q1 q2))
wp [While b i s] q1 q2 = (boolTobool2 i)
wp (s1:sn) q1 q2 = (wp [s1] (wp sn q1 q2) q2)






subBexp :: Boolean2 -> String -> Expr -> Boolean2
subBexp (Greater2 a b) x e = Greater2 (substitute a x e) (substitute b x e)
subBexp (GreaterEqual2 a b) x e = GreaterEqual2 (substitute a x e) (substitute b x e)
subBexp (Less2 a b) x e = Less2 (substitute a x e) (substitute b x e)
subBexp (LessEqual2 a b) x e = LessEqual2 (substitute a x e) (substitute b x e)
subBexp (Equal2 a b) x e = Equal2 (substitute a x e) (substitute b x e)
subBexp (Different2 a b) x e = Different2 (substitute a x e) (substitute b x e)
subBexp (Not2 a) x e = Not2 (subBexp a x e)
subBexp (And2 a b) x e = And2 (subBexp a x e) (subBexp b x e)
subBexp (Orl2 a b) x e = Orl2 (subBexp a x e) (subBexp b x e)
subBexp (Implies2 a b) x e = Implies2 (subBexp a x e) (subBexp b x e)
subBexp a x e = a


substitute:: Expr -> String -> Expr -> Expr
substitute (Var a) x e = if a == x then e
                            else Var a
substitute (Add a b) x e  = Add (substitute a x e) (substitute b x e)
substitute (Mul a b) x e  = Mul (substitute a x e) (substitute b x e)
substitute (Sub a b) x e  = Sub (substitute a x e) (substitute b x e)
substitute (Div a b) x e  = Div (substitute a x e) (substitute b x e)
substitute (Same a b) x e = Same (substitute a x e) (substitute b x e)
substitute a x e = a


script :: SL -> Z3 [Result]
script x = do
    vc <- vcs x
    mapM (\l -> reset >> assert l >> check) vc

main :: IO ()
main = do 
    --putStrLn "Nome do ficheiro a dar parse:"
    --f <- getLine
    --tiny <- parseFile f
    --putStrLn $ lstPrettyPrint (Set.toList $ vcg tiny)

    result <- evalZ3 $ script slt
    mapM_ print result

{-
substitute:: Expr -> String -> Expr -> Expr
substitute (Var s) x e = if s == x then e
                            else Var s

substitute (Neg exp) x e = Neg (substitute exp x e)
substitute (IntBinary op exp1 exp2) x e = IntBinary op (substitute exp1 x e) (substitute exp2 x e)
substitute exp x e = exp
-}
--vcgenerator :: SL -> [Boolean]
--vcgenerator (SL a b c d e f g)


