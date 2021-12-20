import Prelude hiding (reverse)

reverse_helper [] ys = ys
reverse_helper (x:xs) ys = reverse_helper xs (x:ys)

reverse xs = reverse_helper xs []
