module VC where

import AST
import ParserSL
import Z3.Monad

exprZ (Var s) = mkFreshIntVar s

exprZ (Const c) = mkInteger c

exprZ (Add a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkAdd [c, d]
}

exprZ (Mul a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkMul [c, d]
}

exprZ (Div a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkDiv c d
}

exprZ (Sub a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkSub [c, d]
}

booleanZ (Expr2 e) = exprZ e

booleanZ (BoolConst2 e)
      | e = mkTrue
      | otherwise = mkFalse

booleanZ (Less2 a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkLt c d
}

booleanZ (Not2 a) = booleanZ a >>= mkNot


booleanZ (LessEqual2 a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkLe c d
}

booleanZ (Greater2 a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkGt c d
}

booleanZ (GreaterEqual2 a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkGe c d
}

booleanZ (Equal2 a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkEq c d
}

booleanZ (Different2 a b) = do {
  c <- exprZ a;
  d <- exprZ b;
  mkEq c d >>= mkNot
}



booleanZ (And2 a b) = do {
  c <- booleanZ a;
  d <- booleanZ b;
  mkAnd [c,d]
}

booleanZ (Orl2 a b) = do {
  c <- booleanZ a;
  d <- booleanZ b;
  mkOr [c,d]
}

booleanZ (Implies2 a b) = do {
  c <- booleanZ a;
  d <- booleanZ b;
  mkImplies c d
}


gera_vc = sequence . vcgZ . vcg

vcgZ a = if a == [] then [] else map booleanZ a

vcg (Program a b c d e f g) = [(Implies2 (boolTobool2 c) (wp e (boolTobool2 f) (boolTobool2 g) ))]
                              ++ (vcaux e (boolTobool2 f) (boolTobool2 g))



vcaux [] q1 q2 = []
vcaux [Assign v i] q1 q2 = []
vcaux [IfThenElse b s1 s2] q1 q2 = (vcaux s1 q1 q2) ++ (vcaux s2 q1 q2)
vcaux [While b i s] q1 q2 = (([Implies2 (And2 (boolTobool2 i) (boolTobool2 b)) (wp s (boolTobool2 i) q2),
                            Implies2 (And2 (boolTobool2 i) (Not2 (boolTobool2 b))) q1])) ++ (vcaux s (boolTobool2 i) q2)
vcaux [Try s1 s2] q1 q2 = (vcaux s1 q1 q2) ++ (vcaux s2 q1 q2)
vcaux [Throw] q1 q2 = []
vcaux (s1:sn) q1 q2 = (vcaux [s1] (wp sn q1 q2) q2) ++ (vcaux sn q1 q2)


wp [] q1 q2 = q1
wp [Assign x e] q1 q2 = aux1 q1 x e
wp [IfThenElse b s1 s2] q1 q2 = And2 (Implies2 (boolTobool2 b) (wp s1 q1 q2)) (Implies2 (Not2 (boolTobool2 b)) (wp s2 q1 q2))
wp [While b i s] q1 q2 = (boolTobool2 i)
wp [Try s1 s2] q1 q2 = (wp s1 q1 (wp s2 q1 q2))
wp [Throw] q1 q2 = q2
wp (s1:sn) q1 q2 = (wp [s1] (wp sn q1 q2) q2)


aux1 (Greater2 a b) x e = Greater2 (aux2 a x e) (aux2 b x e)
aux1 (GreaterEqual2 a b) x e = GreaterEqual2 (aux2 a x e) (aux2 b x e)
aux1 (Less2 a b) x e = Less2 (aux2 a x e) (aux2 b x e)
aux1 (LessEqual2 a b) x e = LessEqual2 (aux2 a x e) (aux2 b x e)
aux1 (Equal2 a b) x e = Equal2 (aux2 a x e) (aux2 b x e)
aux1 (Different2 a b) x e = Different2 (aux2 a x e) (aux2 b x e)
aux1 (Not2 a) x e = Not2 (aux1 a x e)
aux1 (And2 a b) x e = And2 (aux1 a x e) (aux1 b x e)
aux1 (Orl2 a b) x e = Orl2 (aux1 a x e) (aux1 b x e)
aux1 (Implies2 a b) x e = Implies2 (aux1 a x e) (aux1 b x e)
aux1 a x e = a


aux2 (Var a) x e = if a == x then e
                            else Var a
aux2 (Add a b) x e  = Add (aux2 a x e) (aux2 b x e)
aux2 (Mul a b) x e  = Mul (aux2 a x e) (aux2 b x e)
aux2 (Sub a b) x e  = Sub (aux2 a x e) (aux2 b x e)
aux2 (Div a b) x e  = Div (aux2 a x e) (aux2 b x e)
aux2 (Same a b) x e = Same (aux2 a x e) (aux2 b x e)
aux2 a x e = a


auxPrintVCs []     = []
auxPrintVCs (x:xs) = auxPrintVC x ++ "\n" ++ auxPrintVCs xs


auxPrintVC (Expr2 a) = auxPrintExpr a
auxPrintVC (Greater2 a b) = (auxPrintExpr a) ++ " > " ++ (auxPrintExpr b)
auxPrintVC (GreaterEqual2 a b) = (auxPrintExpr a) ++ " >= " ++ (auxPrintExpr b)
auxPrintVC (Less2 a b) = (auxPrintExpr a) ++ " < " ++ (auxPrintExpr b)
auxPrintVC (LessEqual2 a b) = (auxPrintExpr a) ++ " <= " ++ (auxPrintExpr b)
auxPrintVC (And2 a b) = (auxPrintVC a) ++ " && " ++ (auxPrintVC b)
auxPrintVC (Orl2 a b) = (auxPrintVC a) ++ " || " ++ (auxPrintVC b)
auxPrintVC (Equal2 a b) = (auxPrintExpr a) ++ " == " ++ (auxPrintExpr b)
auxPrintVC (Different2 a b) = (auxPrintExpr a) ++ " != " ++ (auxPrintExpr b)
auxPrintVC (BoolConst2 a) = show a
auxPrintVC (Implies2 a b) = (auxPrintVC a) ++ " ==> " ++ (auxPrintVC b)
auxPrintVC (Not2 a ) = "not (" ++ (auxPrintVC a) ++ ")"

auxPrintExpr (Const a) = show a
auxPrintExpr (Var a) = a
auxPrintExpr (Add a b) = (auxPrintExpr a) ++ " + " ++ (auxPrintExpr b)
auxPrintExpr (Mul a b) = (auxPrintExpr a) ++ " * " ++ (auxPrintExpr b)
auxPrintExpr (Div a b) = (auxPrintExpr a) ++ " / " ++ (auxPrintExpr b)
auxPrintExpr (Sub a b) = (auxPrintExpr a) ++ " - " ++ (auxPrintExpr b)
auxPrintExpr (Same a b) = (auxPrintExpr a) ++ " = " ++ (auxPrintExpr b)

auxBP x = do
    vc <- gera_vc x
    mapM (\l -> reset >> assert l >> check) vc

main = do
    putStrLn $ auxPrintVCs (vcg sl1)
    final <- evalZ3 $ auxBP sl1
    mapM_ print final
