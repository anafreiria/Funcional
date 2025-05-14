fold :: (a->a->a) -> [a] -> a
fold op [a] = a
fold op (a:bx) = op a (fold op bx)

map :: (a->b) -> [a] -> [b]
map f [] = []
map f (a:bx) = f a : map f bx