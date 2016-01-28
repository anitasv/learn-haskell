-- This tries to prove algebraic identities. I am starting with simple
-- multi-variable polynomials, trying to prove stuff like
-- Degen's Formula: https://en.wikipedia.org/wiki/Degen%27s_eight-square_identity
--
-- Eventually I want to be able to generate rational field equations etc.
import Data.List

newtype Sym = Sym [Char] deriving (Eq,Ord)
newtype SymList = SymList [Sym] deriving (Eq, Ord)
data Term = Term Integer SymList deriving Eq
newtype Expr = Expr [Term] deriving Eq

-- Bootstrap methods

var :: [Char] -> Expr
var name = Expr [Term 1 (SymList [Sym name])]

instance Num Expr where

    Expr e1 + Expr e2 = reduce(Expr comb) where
        comb = sort (e1 ++ e2)
        reduce :: Expr -> Expr
        reduce (Expr t) = (Expr (reduceList t)) where
            reduceList [] = []
            reduceList ((Term 0 ls):xs) = reduceList(xs)
            reduceList [t] = [t]
            reduceList ((Term c1 l1):(Term c2 l2):xs)
                | l1 == l2 = reduceList((Term (c1 + c2) l1):xs)
                | otherwise = (Term c1 l1):reduceList((Term c2 l2):xs)

    -- No need to reduce here :)
    Expr e1 * Expr e2 = expn where
        mlist (SymList l1) (SymList l2) = 
            SymList (sort (l1 ++ l2))
        mult (Term c1 l1) (Term c2 l2) =
            Term (c1 * c2) (mlist l1 l2)
        pair1 = [ (t1, t2) | t1 <- e1, t2 <- e2]
        multPair (x, y) = mult x y
        pair2 = map multPair pair1
        promote t = Expr [t]
        prod = map promote pair2
        expn = foldr (+) 0 prod

    negate (Expr e1) = Expr (map inv e1) where 
        inv (Term c ls) = (Term (-c) ls)

    fromInteger x = Expr [Term x (SymList [])]

    -- Isn't there a better way to do this without
    -- having non-sensical abs, and signum?
    abs e = e
    signum e = 1


-- Special Ordering for term.
instance Ord Term where
    Term _ s1 `compare` Term _ s2 = s1 `compare` s2

-- Stuff about showing!
instance Show Sym where
    show (Sym str) = str

instance Show SymList where
    show (SymList ls) = showSym ls where 
        showSym [] = "" 
        showSym [x] = show(x)
        showSym (x:xs) = show(x) ++ "." ++ showSym(xs)

instance Show Term where
    show (Term c ls) = showTerm c ls where
        showTerm c (SymList []) = show(c)
        showTerm 1 ls = show(ls)
        showTerm (-1) ls = "-" ++ show(ls)
        showTerm c ls = show(c) ++ " " ++ show(ls)

instance Show Expr where
    show (Expr t) = showExpr t where
        showExpr [] = "0"
        showExpr [x] = show(x)
        showExpr (x:(Term c vs):xs)
            | c > 0 = show(x) ++ "+" ++ showExpr((Term c vs):xs)
            | c < 0 = show(x) ++ showExpr((Term c vs):xs)
            | otherwise = showExpr (x:xs)

-- Tests
testShow = e where 
    t1 = Term 1 (SymList [Sym "a"])
    t2 = Term (-1) (SymList [Sym "b"])
    e = Expr [t1, t2]

testAdd = e where
    a = var "a"
    b = var "b"
    e = a - b 

testMult = e where
    a = var "a"
    b = var "b"
    e = a * b 

testCoerce = e where
    a = var "a"
    b = var "b"
    e = (1 + b) * (1 - b) 

testForm1 = e where
    a = var "a"
    b = var "b"
    e = (a + b)^2

testForm2 = e where
    a = var "a"
    b = var "b"
    e = (a - b)^2

testForm3 = e where
    a = var "a"
    b = var "b"
    e = (a + b)*(a - b)

testForm4 = e where
    x = var "x"
    a = var "a"
    b = var "b"
    e = (x + a) * (x + b)

testForm5 = e where
    a = var "a"
    b = var "b"
    e = (a^2 - 2*a*b + b^2) * (a^2 + 2*a*b + b^2) 

testEuler = ver where
    a1 = var "a1"; 
    a2 = var "a2"; 
    a3 = var "a3"; 
    a4 = var "a4"; 
    b1 = var "b1"; 
    b2 = var "b2"; 
    b3 = var "b3"; 
    b4 = var "b4"; 

    sa = a1^2 + a2^2 + a3^2 + a4^2;
    sb = b1^2 + b2^2 + b3^2 + b4^2;
    
    c1 = a1 * b1 + a2 * b2 + a3 * b3 + a4 * b4;
    c2 = a1 * b2 - a2 * b1 + a3 * b4 - a4 * b3;
    c3 = a1 * b3 - a2 * b4 - a3 * b1 + a4 * b2;
    c4 = a1 * b4 + a2 * b3 - a3 * b2 - a4 * b1;

    sc = c1^2 + c2^2 + c3^2 + c4^2;

    ver = sa * sb - sc;

testDegen = ver where
    a1 = var "a1" 
    a2 = var "a2" 
    a3 = var "a3" 
    a4 = var "a4" 
    a5 = var "a5" 
    a6 = var "a6" 
    a7 = var "a7" 
    a8 = var "a8" 
    b1 = var "b1" 
    b2 = var "b2" 
    b3 = var "b3" 
    b4 = var "b4" 
    b5 = var "b5" 
    b6 = var "b6" 
    b7 = var "b7" 
    b8 = var "b8" 
    
    cl = [
        a1 * b1 - a2 * b2 - a3 * b3 - a4 * b4 - a5 * b5 - a6 * b6 - a7 * b7 - a8 * b8,
        a1 * b2 + a2 * b1 + a3 * b4 - a4 * b3 + a5 * b6 - a6 * b5 - a7 * b8 + a8 * b7,
        a1 * b3 - a2 * b4 + a3 * b1 + a4 * b2 + a5 * b7 + a6 * b8 - a7 * b5 - a8 * b6,
        a1 * b4 + a2 * b3 - a3 * b2 + a4 * b1 + a5 * b8 - a6 * b7 + a7 * b6 - a8 * b5,
        a1 * b5 - a2 * b6 - a3 * b7 - a4 * b8 + a5 * b1 + a6 * b2 + a7 * b3 + a8 * b4,
        a1 * b6 + a2 * b5 - a3 * b8 + a4 * b7 - a5 * b2 + a6 * b1 - a7 * b4 + a8 * b3,
        a1 * b7 + a2 * b8 + a3 * b5 - a4 * b6 - a5 * b3 + a6 * b4 + a7 * b1 - a8 * b2,
        a1 * b8 - a2 * b7 + a3 * b6 + a4 * b5 - a5 * b4 - a6 * b3 + a7 * b2 + a8 * b1]
  
    al = makevar("a")
    bl = makevar("b") 
    sa = sumsq al 
    sb = sumsq bl 
    sc = sumsq cl

    -- Must return 0 for verification success
    ver = sa * sb - sc 

    makevar p = map var (map (p++) (map show [1..8])) 
    sumsq x = foldr (+) 0 (map (^2) x)
