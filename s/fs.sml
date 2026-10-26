structure Fs = struct
datatype node = File of int list * string list | Dir of (string * node) list
type path = string list
fun lookup [] n = n
  | lookup (h::t) (Dir d) = (case List.find (fn (n,_) => n=h) d of SOME (_,n) => lookup t n | NONE => raise Fail "e")
  | lookup _ _ = raise Fail "e"
fun ls (Dir d) = map #1 d | ls _ = []
fun tags (File (_,t)) = t | tags _ = []
fun content (File (c,_)) = c | content _ = []
fun threat_rm ("SYS.566"::_) t = t+2 | threat_rm _ t = t
fun rm [] (Dir _) = Dir []
  | rm [name] (Dir d) = Dir (List.filter (fn (n,_) => n<>name) d)
  | rm (h::t) (Dir d) = Dir (map (fn (n,v) => if n=h then (n,rm t v) else (n,v)) d)
  | rm _ n = n
fun ins [] name node (Dir d) = Dir ((name,node)::List.filter (fn (n,_) => n<>name) d)
  | ins (h::t) name node (Dir d) =
      let fun go [] = [(h, ins t name node (Dir []))]
            | go ((n,v)::r) = if n=h then (n,ins t name node v)::r else (n,v)::go r
      in Dir (go d) end
  | ins _ _ _ n = n
fun write p name bs fs = ins p name (File (bs,[])) fs
fun init () = Dir [
  ("SYS.566", Dir (
    List.tabulate (8, fn i =>
      ("FRAGMENT.00"^Int.toString (i+1), File ([i+1],["FRAGMENT"]))) @
    [("ENTITY.BIN", File ([0],["ENTITY"]))])),
  ("BIN", Dir []),
  ("RESEARCH", Dir [("PROTOCOL.TXT", File ([0],["LOCKED"]))]),
  ("VAR", Dir [("NEURAL.LOG", File ([],[]))])
]
end
