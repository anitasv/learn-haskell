module Main where

import Data.List as List

-- Solution to problem: https://www.youtube.com/watch?v=xHh0ui5mi_E

equiv [x, y, z] 
    | (x == y) && (y==z) = [6, 6]
    | (x == 2) && (y==3) && (z==5) = [1, 1]
    | (x == 2) && (y==4) && (z==6) = [2, 3]
    | (x == y) = [y, z]
    | (y == z) = [x, y]
    | (x == y - 1) && (y + 1 == z) = [y, y]
    | (x == 1) = [y, z]
    | (x == 2) = [1, y - 1]
    | True = [1, y + 1]

dice = [1..6]

-- We are sorting them to make them "indistinguishable", sorted is a canonical.
three = map sort [ [i, j, k] | i <-dice, j <- dice, k <-dice ]
two = map sort [ [i, j] | i <-dice, j <- dice ]

main = print (actual_freq three == expected_freq two) where
    -- Expected frequency 
    expected_freq = map (mult_freq (length three)) . frequency
    
    -- Actual frequency during the simulation
    actual_freq = map (mult_freq (length two)) . frequency . (map equiv)
    
    frequency list = map (\l -> (length l, head l)) (group (sort list))
    
    mult_freq m (freq, x) = (m * freq, x)
