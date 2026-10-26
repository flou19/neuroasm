structure C = struct
type w = int
type cpu = {r:w V.t, f:w V.t, pc:int, cy:int, m:w V.t, h:bool}
fun gr i (c:cpu) = V.nth i (#r c) handle Subscript => 0
fun sr i x (c:cpu) = {r=V.upd i x (#r c), f= #f c, pc= #pc c, cy= #cy c, m= #m c, h= #h c}
fun gf i (c:cpu) = V.nth i (#f c)
fun sf i x (c:cpu) = {r= #r c, f=V.upd i x (#f c), pc= #pc c, cy= #cy c, m= #m c, h= #h c}
fun gm i (c:cpu) = V.nth i (#m c) handle Subscript => 0
fun sm i x (c:cpu) = {r= #r c, f= #f c, pc= #pc c, cy= #cy c, m=V.upd i x (#m c), h= #h c}
fun step (c:cpu) =
  if #h c then c else
  let val opc = gm (#pc c) c
      val pc1 = #pc c + 1
      val cy1 = #cy c + 1
      fun r j = gr j c
      fun put j x = V.upd j x (#r c)
  in case opc of
       0 => {r= #r c, f= #f c, pc=pc1, cy=cy1, m= #m c, h=false}
     | 1 => let val rs = gm pc1 c val rd = gm (pc1+1) c
            in {r=put rd (r rs), f= #f c, pc=pc1+2, cy=cy1, m= #m c, h=false} end
     | 2 => let val rd = gm pc1 c val imm = gm (pc1+1) c
            in {r=put rd imm, f= #f c, pc=pc1+2, cy=cy1, m= #m c, h=false} end
     | 3 => let val rs = gm pc1 c val rd = gm (pc1+1) c
            in {r=put rd (r rd + r rs), f= #f c, pc=pc1+2, cy=cy1, m= #m c, h=false} end
     | 4 => let val rs = gm pc1 c val rd = gm (pc1+1) c
            in {r=put rd (r rd - r rs), f= #f c, pc=pc1+2, cy=cy1, m= #m c, h=false} end
     | 8 => {r= #r c, f= #f c, pc=gm pc1 c, cy=cy1, m= #m c, h=false}
     | 13 => {r= #r c, f= #f c, pc=pc1, cy=cy1, m= #m c, h=true}
     | 14 => let val rd = gm pc1 c
             in {r=put rd (r rd + 1), f= #f c, pc=pc1+1, cy=cy1, m= #m c, h=false} end
     | 15 => let val rd = gm pc1 c
             in {r=put rd (r rd - 1), f= #f c, pc=pc1+1, cy=cy1, m= #m c, h=false} end
     | _ => {r= #r c, f= #f c, pc=pc1, cy=cy1, m= #m c, h=false}
  end
fun run 0 c = c
  | run n c = if #h c then c else run (n-1) (step c)
fun cy (c:cpu) = #cy c
fun halted (c:cpu) = #h c
fun init () = {r=V.tab 8 (fn _ => 0), f=V.tab 4 (fn _ => 0), pc=0, cy=0, m=V.tab 256 (fn _ => 0), h=false}
end
