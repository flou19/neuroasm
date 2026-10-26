structure Io = struct
open St
fun out s = TextIO.output (TextIO.stdOut, s)
fun status (s:t) =
  out ("T=" ^ Int.toString (#th s) ^ " N=" ^ Int.toString (#n s) ^
       " F=" ^ Int.toString (length (#fr s)) ^
       " SU=" ^ (case #su s of Sleep => "0" | Hunt _ => "1") ^
       " EG=" ^ (case #eg s of Nil=>"-" | Esc=>"E" | Iso=>"I" | Rev=>"R" | Obs=>"O" | Catch=>"X") ^ "\n")
fun dispatch (s:t) tok : t =
  let val s = observe (decay s)
  in case tok of
       ["LS"] => (out (String.concatWith "\n" (Fs.ls (Fs.lookup (#cd s) (#fs s))) ^ "\n"); log "LS" s)
     | ["PWD"] => (out ("/" ^ String.concatWith "/" (#cd s) ^ "\n"); log "PWD" s)
     | "CD"::p => (let val p' = if p=[".."] then (case #cd s of []=>[] | xs=>List.take(xs,length xs-1)) else (#cd s)@p
                   in (Fs.lookup p' (#fs s);
                       log "CD" {cpu= #cpu s, net= #net s, th= #th s, su= #su s, maze= #maze s,
                                 pl= #pl s, en= #en s, fs= #fs s, cd=p', k= #k s,
                                 fr= #fr s, hi= #hi s, n= #n s, rev= #rev s, eg= #eg s})
                      handle Fail _ => (out "e\n"; log "CD?" s)
                   end)
     | "RM"::p => rm s p
     | ["W"] => mv s 0 | ["S"] => mv s 1 | ["A"] => mv s 2 | ["D"] => mv s 3
     | ["ISO"] => iso s
     | "COL"::n::_ => col s n
     | ["REV"] => reveal s
     | "RUN"::n::_ => (case Int.fromString n of SOME k => let val s'=run s k in out ("CY=" ^ Int.toString (C.cy (#cpu s')) ^ "\n"); s' end | _ => s)
     | ["ST"] => (status s; log "ST" s)
     | ["H"] => (out "LS CD PWD RM W/A/S/D ISO COL REV RUN ST H Q\n"; log "H" s)
     | ["Q"] => (out ".\n"; OS.Process.exit OS.Process.success; s)
     | _ => (out "?\n"; s)
  end
fun repl (s:t) =
  if #eg s <> Nil then out ("EG " ^ (case #eg s of Esc=>"ESC" | Iso=>"ISO" | Rev=>"REV" | Obs=>"OBS" | Catch=>"CATCH" | _=>"?") ^ "\n")
  else (out "> "; case TextIO.inputLine TextIO.stdIn of
          NONE => ()
        | SOME l => repl (dispatch s (String.tokens Char.isSpace (String.substring (l,0,size l-1)))))
fun main () = (out "na\n"; repl (init ()))
end
