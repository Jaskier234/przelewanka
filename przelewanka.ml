

(* dozwalone operacje na stanach *)
(* ======================================================= *)
let nalej maks stan i =
  let a = Array.copy stan in
  a.(i) <- maks.(i); a

let wylej stan i =
  let a = Array.copy stan in
  a.(i) <- 0; a

let przelej maks stan i j =
  let delta = min stan.(i) (maks.(j) - stan.(j)) in
  let a = Array.copy stan in
  a.(i) <- a.(i) - delta;
  a.(j) <- a.(j) + delta; a
(* ======================================================= *)

let kompresuj maks stan = (* bijekcja ze stanu w int *)
  let ans = ref 0 in
  let ilo = ref 1 in
  for i=0 to Array.length stan - 1 do
    ans := !ans + (stan.(i) * !ilo);
    ilo := !ilo * (maks.(i) + 1);
  done; !ans

let rec nwd a b =
  if b = 0 then a else nwd b (a mod b)

let konieczny maks cel =
  if Array.length maks = 0 then true else
  let dziel = Array.fold_left nwd maks.(0) maks in
  if dziel = 0 then true else
    let f n x =
      if x mod dziel = 0 then n else false
    and g (i, z) x =
      (i + 1, if x = 0 || x = maks.(i) then true else z) in
    (* czy nwd pojemności dzieli wszystkie poziomy docelowego stanu *)
    let n = Array.fold_left f true cel in
    (* czy w stanie docelowym jest pusty lub pełny *)
    let (_, z) = Array.fold_left g (0, false) cel in
    n && z

let przelewanka arr =
  let n = Array.length arr in
  let maks = Array.map (fun (x, y) -> x) arr in 
  let cel = Array.map (fun (x, y) -> y) arr in
  if konieczny maks cel = false then -1 else
  let start = Array.make n 0 in
  let queue = Queue.create () in
  let odw = Hashtbl.create 100000 in
  
  let push (s, d) q = (* dodanie stanu na kolejkę, jeśli nie występuje *)
    let l = kompresuj maks s in
    if Hashtbl.mem odw l = false
    then Queue.push (s, d) q; Hashtbl.add odw l true
  in
  push (start, 0) queue; (* stan początkowy *)

  let rec search () =
    if Queue.is_empty queue then -1 else
      begin
        let (u, d) = Queue.pop queue in
        if u = cel then d else
          begin
            for i = 0 to n - 1 do
              push (nalej maks u i, d + 1) queue;
              push (wylej u i, d + 1) queue;
            done;
            for i = 0 to n - 1 do
              for j = 0 to n - 1 do
                push (przelej maks u i j, d + 1) queue;
              done;
            done;
            search ()
          end
      end
  in
  search ()

