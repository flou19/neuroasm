structure Gate = struct
fun check () =
  let fun try p =
        let val ins = TextIO.openIn p
            val s = TextIO.inputAll ins
            val _ = TextIO.closeIn ins
        in String.isSubstring "OK" s end
      handle _ => false
  in if try "t/.ok" orelse try "../t/.ok" orelse try "/home/workdir/artifacts/neuroasm/t/.ok"
     then () else raise Fail "twelf"
  end
end
