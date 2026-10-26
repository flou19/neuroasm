structure St = struct
datatype su = Sleep | Hunt of G.p * G.p
datatype endg = Nil | Esc | Iso | Rev | Obs | Catch
type t = {
  cpu:C.cpu, net:Nn.net, th:int, su:su, maze:G.maze,
  pl:G.p, en:G.p, fs:Fs.node, cd:Fs.path, k:int,
  fr:string list, hi:string list, n:int, rev:bool, eg:endg
}
fun pure (s:t) =
  {cpu= #cpu s, net= #net s, th= #th s, su= #su s, maze= #maze s,
   pl= #pl s, en= #en s, fs= #fs s, cd= #cd s, k= #k s,
   fr= #fr s, hi= #hi s, n= #n s, rev= #rev s, eg= #eg s}
fun log line (s:t) : t =
  {cpu= #cpu s, net= #net s, th= #th s, su= #su s, maze= #maze s,
   pl= #pl s, en= #en s, fs= #fs s, cd= #cd s, k= #k s,
   fr= #fr s, hi=line::(#hi s), n= #n s+1, rev= #rev s, eg= #eg s}
fun with_eg e (s:t) : t =
  {cpu= #cpu s, net= #net s, th= #th s, su= #su s, maze= #maze s,
   pl= #pl s, en= #en s, fs= #fs s, cd= #cd s, k= #k s,
   fr= #fr s, hi= #hi s, n= #n s, rev= #rev s, eg= e}
fun wake t Sleep = if t >= 3 then Hunt ((6,6),(1,1)) else Sleep | wake _ u = u
fun decay (s:t) : t =
  if #th s > 0 andalso #n s mod 15 = 0 andalso (case #su s of Sleep => true | _ => false)
  then {cpu= #cpu s, net= #net s, th= #th s - 1, su= #su s, maze= #maze s,
        pl= #pl s, en= #en s, fs= #fs s, cd= #cd s, k= #k s,
        fr= #fr s, hi= #hi s, n= #n s, rev= #rev s, eg= #eg s}
  else s
fun observe (s:t) =
  if #n s >= 60 andalso #th s <= 1 then with_eg Obs s else s
fun init () : t = {
  cpu=C.init (), net=Nn.init (), th=0, su=Sleep, maze=G.init (),
  pl=(1,1), en=(6,6), fs=Fs.init (), cd=[], k=0,
  fr=[], hi=[], n=0, rev=false, eg=Nil
}
fun rm (s:t) p =
  let val full = (#cd s)@p
      val th' = Fs.threat_rm full (#th s)
      val fs' = Fs.rm full (#fs s)
      val su' = wake th' (#su s)
  in log ("RM") {cpu= #cpu s, net= #net s, th=th', su=su', maze= #maze s,
                 pl= #pl s, en= #en s, fs=fs', cd= #cd s, k= #k s,
                 fr= #fr s, hi= #hi s, n= #n s, rev= #rev s, eg= #eg s}
  end
fun mv (s:t) d =
  let val pl' = G.shift (#pl s) d
  in if not (G.valid pl' (#maze s)) then log "MV?" s
     else if pl' = #en s andalso (case #su s of Hunt _ => true | _ => false)
       then with_eg Catch (log "CATCH" s)
     else if G.get pl' (#maze s) = 2
       then with_eg Esc (log "ESC" {cpu= #cpu s, net= #net s, th= #th s, su= #su s, maze= #maze s,
         pl=pl', en= #en s, fs= #fs s, cd= #cd s, k= #k s+1,
         fr= #fr s, hi= #hi s, n= #n s, rev= #rev s, eg= #eg s})
     else
       let val en' = case #su s of Hunt _ => G.predict pl' (#en s) (#maze s) | _ => #en s
           val sg = Nn.threat_signal (#net s) (#th s)
           val th' = if sg > 5 andalso #th s < 9 then #th s + 1 else #th s
       in log "MV" {cpu= #cpu s, net= #net s, th=th', su= #su s, maze= #maze s,
                    pl=pl', en=en', fs= #fs s, cd= #cd s, k= #k s+1,
                    fr= #fr s, hi= #hi s, n= #n s, rev= #rev s, eg= #eg s}
       end
  end
fun iso (s:t) =
  if #th s >= 2 then with_eg Iso (log "ISO" {cpu= #cpu s, net= #net s, th= #th s, su=Sleep, maze= #maze s,
    pl= #pl s, en= #en s, fs= #fs s, cd= #cd s, k= #k s,
    fr= #fr s, hi= #hi s, n= #n s, rev= #rev s, eg= #eg s})
  else log "ISO?" s
fun col (s:t) name =
  let val n = Fs.lookup ((#cd s)@[name]) (#fs s)
      val ok = List.exists (fn t => t="FRAGMENT") (Fs.tags n) orelse String.isSubstring "FRAGMENT" name
  in if ok then
       let val fr = if List.exists (fn x => x=name) (#fr s) then #fr s else name::(#fr s)
       in log ("COL") {cpu= #cpu s, net= #net s, th= #th s, su= #su s, maze= #maze s,
                       pl= #pl s, en= #en s, fs= #fs s, cd= #cd s, k= #k s,
                       fr=fr, hi= #hi s, n= #n s, rev= #rev s, eg= #eg s}
       end
     else log "COL?" s
  end handle Fail _ => log "COL?" s
fun reveal (s:t) =
  if length (#fr s) >= 8 then with_eg Rev (log "REV" {cpu= #cpu s, net= #net s, th= #th s, su= #su s, maze= #maze s,
    pl= #pl s, en= #en s, fs= #fs s, cd= #cd s, k= #k s,
    fr= #fr s, hi= #hi s, n= #n s, rev=true, eg= #eg s})
  else log "REV?" s
fun run (s:t) k =
  let val c' = C.run k (#cpu s)
  in log ("RUN") {cpu=c', net= #net s, th= #th s, su= #su s, maze= #maze s,
                  pl= #pl s, en= #en s, fs= #fs s, cd= #cd s, k= #k s,
                  fr= #fr s, hi= #hi s, n= #n s, rev= #rev s, eg= #eg s}
  end
end
