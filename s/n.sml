structure N = struct
type t = int
val z = 0
val s = fn n => n + 1
val add = fn a => fn b => a + b
val sub = fn a => fn b => Int.max (0, a - b)
val mul = fn a => fn b => a * b
val le = fn a => fn b => a <= b
val lt = fn a => fn b => a < b
val eq = fn (a:int) => fn b => a = b
val isz = fn 0 => 1 | _ => 0
end
