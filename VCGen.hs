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

boolExprToZ3 (Boolean e)
      | e = mkTrue
      | otherwise = mkFalse

boolExprToZ3 (Less a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkLt c d
}

boolExprToZ3 (LessEqual a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkLe c d
}

boolExprToZ3 (Greater a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkGt c d
}

boolExprToZ3 (GreaterEqual a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkGe c d
}

boolExprToZ3 (Equal a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkEq c d
}

boolExprToZ3 (Different a b) = do {
  c <- intExprToZ3 a;
  d <- intExprToZ3 b;
  mkEq c d >>= mkNot
}

--vcgenerator :: SL -> [Boolean]
--vcgenerator (SL a b c d e f g)

{--wp :: [Inst] -> Boolean -> Boolean -> Boolean
wp [] pn pe = pn
wp [Throw] pn pe = pe
wp [Assign s e] pn pe = subExp
wp [Try i1 i2] pn pe = (wp i1 pn (wp i2 pn pe))
wp [IfThenElse c i1 i2] pn pe = And (Implies c (wp i1 pn pe)) (Implies (Not c) (wp i2 pn pe))
--}
