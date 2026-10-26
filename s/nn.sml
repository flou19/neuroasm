structure Nn = struct
type n = int V.t * int
type net = n V.t * n V.t * n V.t
fun act x = if x > 0 then x else 0
fun dot (ws, xs) = V.foldl (op+) 0 (V.map (fn (w,x) => w*x) (let
      fun zip (V.C (a,as'), V.C (b,bs')) = V.C ((a,b), zip (as',bs'))
        | zip _ = V.N
      in zip (ws, xs) end handle _ => V.N))
fun fwd1 (w,b) x = act (dot (w,x) + b)
fun fwdl layer x = V.map (fn n => fwd1 n x) layer
fun forward (h1,h2,out) x = fwdl out (fwdl h2 (fwdl h1 x))
fun threat_signal net t =
  let val x = V.fromList [t, t*2, if t>=3 then 1 else 0, 1]
      val y = forward net x
  in V.nth 0 y end
fun init () =
  (V.fromList [(V.fromList [1,~2,3,~1],0),(V.fromList [~1,2,~1,1],1)],
   V.fromList [(V.fromList [1,0],0),(V.fromList [0,1],~1)],
   V.fromList [(V.fromList [2,~1],0)])
end
