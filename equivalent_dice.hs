module Main where

import Data.List as List

-- Solution to problem: https://www.youtube.com/watch?v=xHh0ui5mi_E

equiv [x, y, z] 
    | (x == y) && (y == z) = [6, 6]
    | (x == y) = [y, z]
    | (y == z) = [x, y]
    | (x == y - 1) && (y + 1 == z) = [y, y]

equiv [2, 3, 5] = [1, 1]
equiv [2, 4, 6] = [2, 3]
equiv [1, x, y] = [x, y]
equiv [2, x, _] = [1, x - 1]
equiv [_, x, _] = [1, x + 1]

dice = [1..6]

-- We are sorting them to make them "indistinguishable", sorted is a canonical.
three = map sort [ [i, j, k] | i <-dice, j <- dice, k <-dice ]
two = map sort [ [i, j] | i <-dice, j <- dice ]

main = print (actual_freq three == expected_freq two) where
    -- Expected frequency 
    expected_freq = map (mult_freq (length three)) . frequency
    
    -- Actual frequency during the simulation
    actual_freq = map (mult_freq (length two)) . frequency . (map equiv)
    
    frequency = map (\l -> (length l, head l)) . group . sort
    
    mult_freq m (freq, x) = (m * freq, x)
