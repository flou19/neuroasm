structure V = struct
datatype 'a t = N | C of 'a * 'a t
fun nth 0 (C (x,_)) = x | nth i (C (_,xs)) = nth (i-1) xs | nth _ N = raise Subscript
fun upd 0 x (C (_,xs)) = C (x,xs) | upd i x (C (y,xs)) = C (y, upd (i-1) x xs) | upd _ x N = C (x,N)
fun len N = 0 | len (C (_,xs)) = 1 + len xs
fun tab n f = let fun g i = if i >= n then N else C (f i, g (i+1)) in g 0 end
fun fromList xs = List.foldr (fn (x,a) => C (x,a)) N xs
fun toList N = [] | toList (C (x,xs)) = x :: toList xs
fun map f N = N | map f (C (x,xs)) = C (f x, map f xs)
fun foldl f z N = z | foldl f z (C (x,xs)) = foldl f (f (x,z)) xs
end
