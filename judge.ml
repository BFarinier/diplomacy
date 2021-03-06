(* http://diplom.org/Zine/S2009M/Kruijswijk/DipMath_Chp1.htm *)

module type Core =
sig
  type result = Fails | Succeeds
  type state = Unresolved | Guessing | Resolved
  type 'a t

  val fails : 'a -> 'a t -> 'a t
  val succeeds : 'a -> 'a t -> 'a t

  val unresolved : 'a -> 'a t -> 'a t
  val guessing : 'a -> 'a t -> 'a t
  val resolved : 'a -> 'a t -> 'a t

  val result : 'a -> 'a t -> result
  val state : 'a -> 'a t -> state
  val length : 'a t -> int

  val create : 'a list -> 'a t
  val depend : 'a -> 'a t -> 'a list * 'a t

  val solved : 'a -> 'a t -> 'a t
  val unsolved : 'a -> 'a t -> 'a t

  val resolve :
    ('a t -> 'a -> result) -> ('a t -> 'a -> 'a t) -> 'a t -> 'a -> result
end

module C : Core =
struct
  type result = Fails | Succeeds
  type state = Unresolved | Guessing | Resolved

  type 'a t = {
    resolution : result array;
    state : state array;
    depend : 'a array;
    mutable length : int;
    get : 'a -> int;
  }


  let create (type a) (l: a list) : a t =
    let n = List.length l in
    let module M = Map.Make
        (struct
          type t = a
          let compare = compare
        end)
    in
    let m =
      let i = ref 0 in
      List.fold_left
        (fun acc x -> M.add x (incr i; !i) acc)
        M.empty l
    in
    {
      resolution = Array.make n Fails;
      state = Array.make n Unresolved;
      depend = Array.make n (Obj.magic ());
      length = 0;
      get = fun x -> M.find x m
    }


  let push t x =
    t.depend.(t.length) <- x;
    t.length <- t.length + 1

  let pop t =
    t.length <- t.length - 1;
    t.depend.(t.length)


  let set_resolution r x t =
    t.resolution.(t.get x) <- r; t
  let fails x t =
    set_resolution Fails x t
  let succeeds x t =
    set_resolution Succeeds x t

  let set_state r x t =
    t.state.(t.get x) <- r; t
  let unresolved x t =
    set_state Unresolved x t
  let guessing x t =
    set_state Guessing x t
  let resolved x t=
    set_state Resolved x t

  let result x t = t.resolution.(t.get x)
  let state x t = t.state.(t.get x)
  let length t = t.length


  let depend x t =
    let rec aux n acc =
      if n < t.length then
        aux (n+1) (t.depend.(n) :: acc)
      else
        List.rev acc
    in
    let n = t.get x in
    if n < 0 then [], t else aux n [], t

  let solved x t =
    let rec aux n t =
      if n < t.length then
        let x = pop t in
        let t = resolved x t in
        aux n t
      else t
    in aux (t.get x) t

  let unsolved x t =
    let rec aux n t =
      if n < t.length then
        let x = pop t in
        let t = unresolved x t in
        aux n t
      else t
    in aux (t.get x) t


  let rec resolve adjudicate backup t nr =
    if t.state.(t.get nr) = Resolved
    then begin
      t.resolution.(t.get nr)
    end
    else if t.state.(t.get nr) = Guessing
    then begin
      let res = ref false in
      for i = 0 to t.length - 1 do
        res := !res || t.depend.(i) = nr;
      done;
      if not (!res) then
        push t nr;
      t.resolution.(t.get nr)
    end
    else begin
      let old_length = t.length in
      t.resolution.(t.get nr) <- Fails;
      t.state.(t.get nr) <- Guessing;
      let first = adjudicate t nr in
      if (t.length = old_length)
      then begin
        if t.state.(t.get nr) <> Resolved then
          (t.resolution.(t.get nr) <- first;
           t.state.(t.get nr) <- Resolved);
        first
      end
      else if t.depend.(old_length) <> nr
      then begin
        push t nr;
        first
      end
      else begin
        let t = unsolved t.depend.(old_length) t in
        t.resolution.(t.get nr) <- Succeeds;
        t.state.(t.get nr) <- Guessing;
        let second = adjudicate t nr in
        if first = second
        then begin
          let t = unsolved t.depend.(old_length) t in
          t.resolution.(t.get nr) <- first;
          t.state.(t.get nr) <- Resolved;
          first
        end 
        else begin
          let t = backup t t.depend.(old_length) in
          resolve adjudicate backup t nr
        end
      end
    end

end

open C

let backup t x =
  let l, t = depend x t in
  List.fold_left (fun t x -> fails x t) t l
  |> solved x
