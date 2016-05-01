{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

data FieldError = DivByZero -- Divided by zero.
    | IncompatibleBase 

instance Show FieldError where
    show DivByZero = "Divided by a multiple of base"
    show IncompatibleBase = "Can't add with incompabile bases"

instance Show Field where
    show (Field x y) = (show y) ++ " (mod " ++ (show x) ++ ")"

-- Field 17 3 == 3 (mod 17)
data Field = Field Integer Integer
data Eith a b = ELeft a | ERight b
type FieldMonad = Eith FieldError

instance Show (FieldMonad Field) where
    show (ELeft e) = "Error: " ++ (show e)
    show (ERight v) = "Value: " ++ (show v)

instance Monad FieldMonad where 
    return x = ERight x 
    (ELeft x) >>= f = (ELeft x) 
    (ERight x) >>= f = (f x)

throwError = ELeft

pmod a b | r >= 0 = r
         | otherwise = (r + b)
  where r = a `mod` b

addField m1 m2 = do
    (Field p1 a1) <- m1
    (Field p2 a2) <- m2
    if (p1 == p2)
    then return $ Field p1 $ (a1 + a2) `pmod` p1
    else throwError IncompatibleBase

multField m1 m2 = do
    (Field p1 a1) <- m1
    (Field p2 a2) <- m2
    if (p1 == p2)
    then return $ Field p1 $ (a1 * a2) `pmod` p1
    else throwError IncompatibleBase

negateField m1 = do
    (Field p1 a1) <- m1
    return $ Field p1 $ (p1-a1) `pmod` p1

val x y = ERight $ Field x y

val17 = val 17

instance Num (FieldMonad Field) where
    (+) = addField

    (*) = multField
    
    negate m = negateField m

    abs x = x

    signum x = 1

    fromInteger x = val 0 x

main = print (addField m1 m2) where
    m1 = val17 3
    m2 = val17 15 
