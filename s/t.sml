structure T = struct
fun ok true = () | ok false = raise Fail "T"
fun go () =
  let val _ = Gate.check ()
      val c = C.init ()
      val _ = ok (C.cy (C.run 0 c) = 0)
      val _ = ok (C.cy (C.run 100 c) = 100)
      val _ = ok (C.halted (C.run 3 (C.sm 0 13 (C.init ()))))
      val _ = ok (C.cy (C.run 3 (C.sm 0 13 (C.init ()))) = 1)
      val s = St.init ()
      val s = St.rm s ["SYS.566"]
      val _ = ok (#th s = 2)
      val s = St.rm s ["SYS.566"]
      val _ = ok (case #su s of St.Hunt _ => true | _ => false)
      val s = St.iso s
      val _ = ok (#eg s = St.Iso)
      val s0 = St.init ()
      val s0 = {cpu= #cpu s0, net= #net s0, th= #th s0, su= #su s0, maze= #maze s0,
                pl= #pl s0, en= #en s0, fs= #fs s0, cd=["SYS.566"], k= #k s0,
                fr= #fr s0, hi= #hi s0, n= #n s0, rev= #rev s0, eg= #eg s0}
      val s0 = List.foldl (fn (i,s) => St.col s ("FRAGMENT.00"^Int.toString i)) s0 [1,2,3,4,5,6,7,8]
      val _ = ok (length (#fr s0) = 8)
      val s0 = St.reveal s0
      val _ = ok (#eg s0 = St.Rev)
  in print "T_OK\n" end
end
