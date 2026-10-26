structure G = struct
type p = int * int
type maze = int V.t V.t
fun get (x,y) m = V.nth x (V.nth y m) handle Subscript => 0
fun valid (x,y) m =
  y >= 0 andalso y < V.len m andalso x >= 0 andalso x < V.len (V.nth y m) andalso get (x,y) m <> 0
fun shift (x,y) 0 = (x,y-1) | shift (x,y) 1 = (x,y+1) | shift (x,y) 2 = (x-1,y) | shift (x,y) 3 = (x+1,y) | shift p _ = p
fun nbrs p m = List.filter (fn q => valid q m) [shift p 0, shift p 1, shift p 2, shift p 3]
fun h (x1,y1) (x2,y2) = Int.abs (x1-x2) + Int.abs (y1-y2)
fun best start goal m =
  let val opts = nbrs start m
      fun score q = h q goal
  in case opts of
       [] => start
     | xs => List.foldl (fn (q,b) => if score q < score b then q else b) (hd xs) xs
  end
fun predict pl en m =
  let val opts = nbrs en m
      fun score e' =
        let val d = h e' pl
            val cut = List.foldl (fn (p,a) => Int.min (a, h e' p)) 99 (nbrs pl m)
        in d + cut div 2
        end
  in case opts of [] => en | xs => List.foldl (fn (q,b) => if score q < score b then q else b) (hd xs) xs
  end
fun init () =
  V.fromList [
    V.fromList [0,0,0,0,0,0,0,0],
    V.fromList [0,1,1,1,0,1,1,0],
    V.fromList [0,1,0,1,1,1,0,0],
    V.fromList [0,1,1,0,0,1,1,0],
    V.fromList [0,0,1,1,1,0,1,0],
    V.fromList [0,1,1,0,1,1,1,0],
    V.fromList [0,1,0,1,1,0,2,0],
    V.fromList [0,0,0,0,0,0,0,0]
  ]
end
