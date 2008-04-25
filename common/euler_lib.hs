module EulerLib where

enumerate :: [a] -> [(Int, a)]
enumerate xs = enum_closure xs 0
    where
    enum_closure [] _ = []
    enum_closure (y:ys) n = (n, y) : enum_closure ys (n+1)

cut_head :: [a] -> [a]
cut_head (x:xs) = xs

cut_last :: [a] -> [a]
cut_last = reverse . cut_head . reverse
