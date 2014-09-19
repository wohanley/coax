module Math where

max : Int -> Int -> Int
max = compare (>)

min : Int -> Int -> Int
min = compare (<)

compare : (a -> a -> Bool) -> a -> a -> a
compare f x y = if f x y then x else y