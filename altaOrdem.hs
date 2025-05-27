import Data.List

fold :: (a -> a -> a) -> [a] -> a
fold _ []     = error "Lista vazia"
fold _ [a]    = a
fold op (a:bx) = op a (fold op bx)


myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs
