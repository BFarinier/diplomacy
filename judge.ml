(* http://diplom.org/Zine/S2009M/Kruijswijk/DipMath_Chp1.htm *)

type result = Fails | Succeeds
type state = Unresolved | Guessing | Resolved

type 'a t = {
  resolution : result array;
  state : state array;
  depend : 'a array;
  mutable length : int;
}

let push t n =
  t.depend.(t.length) <- n;
  t.length <- t.length + 1

let cleanup t old =
  while t.length > old do
    t.length <- t.length - 1;
    t.state.(t.length) <- Unresolved
  done

let create n : 'a t = {
  resolution = Array.make n Fails;
  state = Array.make n Unresolved;
  depend = Array.make n (Obj.magic ());
  length = 0;
}

let backup t nr =
  for k = nr to t.length - 1 do
    t.resolution.(k) <- Fails;
    t.state.(k) <- Resolved;
    t.length <- nr
  done

let get : 'a -> int = fun x -> Obj.magic x

let rec resolve adjudicate t nr =
  if t.state.(get nr) = Resolved
  then begin
    t.resolution.(get nr)
  end
  else if t.state.(get nr) = Guessing
  then begin
    let res = ref false in
    for i = 0 to t.length - 1 do
      res := !res || t.depend.(i) = nr;
    done;
    if not (!res) then
      push t nr;
    t.resolution.(get nr)
  end
  else begin
    let old_length = t.length in
    t.resolution.(get nr) <- Fails;
    t.state.(get nr) <- Guessing;
    let first = adjudicate t nr in
    if (t.length = old_length)
    then begin
      if t.state.(get nr) <> Resolved then
        (t.resolution.(get nr) <- first;
         t.state.(get nr) <- Resolved);
      first
    end
    else if t.depend.(old_length) <> nr
    then begin
      push t nr;
      first
    end
    else begin
      cleanup t old_length;
      t.resolution.(get nr) <- Succeeds;
      t.state.(get nr) <- Guessing;
      let second = adjudicate t nr in
      if first = second
      then begin
        cleanup t old_length;
        t.resolution.(get nr) <- first;
        t.state.(get nr) <- Resolved;
        first
      end 
      else begin
        backup t old_length;
        resolve adjudicate t nr
      end
    end
  end
