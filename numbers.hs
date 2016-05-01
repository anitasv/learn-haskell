{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where
import Debug.Trace

data FieldError = DivByZero -- Divided by zero.
    | DivInNat -- Division in natural space.
    | IncompatibleBase 

instance Show FieldError where
    show DivByZero = "Divided by a multiple of base"
    show IncompatibleBase = "Can't add with incompabile bases"

instance Show Field where
    show (Field x y) = show y ++ " (mod " ++ show x ++ ")"

-- Field 17 3 == 3 (mod 17)
data Field = Field Integer Integer
type FieldMonad = Either FieldError

throwError = Left

pmod a b | r >= 0 = r
         | otherwise = r + b
  where r = a `mod` b

pow :: FieldMonad Field -> Integer -> FieldMonad Field
pow x 0 = x >>= \(Field p x) -> return (Field p 1)
pow x y = if y `mod` 2 == 1
          then x * z
          else z where
    z = pow (x * x) (y `div` 2)

addField m1 m2 = do
    (Field p1 a1) <- m1
    (Field p2 a2) <- m2
    if p1 == p2
    then return $ Field p1 $ (a1 + a2) `pmod` p1
    else throwError IncompatibleBase

multField m1 m2 = do
    (Field p1 a1) <- m1
    (Field p2 a2) <- m2
    if p1 == p2
    then return $ Field p1 $ (a1 * a2) `pmod` p1
    else throwError IncompatibleBase

negateField m1 = do
    (Field p1 a1) <- m1
    return $ Field p1 $ (p1-a1) `pmod` p1

inverseField m1 = do
    (Field p1 a1) <- m1
    pow m1 (p1 - 2)

divField m1 m2 = m1 * (inverseField m2)

val x y = return $ Field x y

instance Num (FieldMonad Field) where
    (+) = addField

    (*) = multField
    
    negate = negateField

    abs x = x

    signum _ = 1

    fromInteger x = val 0 x

main = print (divField m1 m2) where
    m1 = val 17 3
    m2 = val 17 2

