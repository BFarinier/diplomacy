(* http://www.floc.net/dpjudge/?page=Algorithm *)

open MapBoard
open Order

type mark =
    Nothing | Void  | NoConvoy
  | ConvoyEndangered | ConvoyUnderAttack
  | Bounce | Cut | Dislodged

type data_units = {
  order : order;
  mutable mark : mark;
  mutable count : int;
}

type data_province = {
  province : any province;
  mutable combats : order list;
}

type data_convoy = data_units list

let set_mark u m = u.mark <- m
let set_count u c = u.count <- c

module SU = Set.Make(struct type t = data_units let compare = compare end)
module SP = Set.Make(struct type t = data_province let compare = compare end)

(* -------------------------------------------------------------------------- *)

module Step1 = struct

  let check_visited p visited not_visited =
    let b = not (List.mem p visited) in
    assert (List.mem p not_visited = b); b

  let update_visited p p' visited not_visited =
    p', p::visited, List.filter (fun p' -> not (p = p')) not_visited

  let rec next_fleets
      (visited: fleets province list)
      (not_visited: fleets province list)
      (p : fleets province)
      (acc: (fleets province * fleets province list * fleets province list) list)
    = function
      | [] -> acc
      | p'::l ->
        if are_adjacent p p' && check_visited p' visited not_visited
        then next_fleets visited not_visited p ((update_visited p p' visited not_visited)::acc) l
        else next_fleets visited not_visited p acc l

  let rec exists_chain
      (source: fleets province)
      (target: fleets province)
      (visited: fleets province list)
      (not_visited: fleets province list) =
    (are_adjacent source target)
    || (not (not_visited = [])
        && let nexts = next_fleets visited not_visited source [] not_visited in
        List.exists (fun (s, visited, not_visited) -> exists_chain s target visited not_visited) nexts)

  let check_convoy
      (source: armies province)
      (target: armies province)
      (conveyors: fleets province list) =
    let source =
      match from_any (to_any source) with
      | None, Some s, None -> water_to_fleets s
      | None, None, Some s -> coastal_to_fleets s
      | _ -> assert false
    in
    let target =
      match from_any (to_any target) with
      | None, Some t, None -> water_to_fleets t
      | None, None, Some t -> coastal_to_fleets t
      | _ -> assert false
    in
    exists_chain source target [] conveyors

  let rec split_convoy_order
      (acc: (armies province * armies province * SU.t) list)
      (su: SU.t) =
    if SU.is_empty su then acc
    else match (SU.choose su).order with
      | FConvoy (_,s,t) ->
        let (su1, su2) = SU.partition (fun u -> match u.order with FConvoy (_,s',t') -> s = s' && t = t' | _ -> false) su in
        split_convoy_order ((s,t,su1)::acc) su2
      | _ -> assert false


  let get_convoy_orders (su: SU.t) =
    let fleets =
      SU.filter (fun u -> match u.order with FConvoy _ -> true | _ -> false ) su
      |> split_convoy_order []
    in
    let armies = SU.filter (fun u -> is_convoying u.order) su in
    fleets, armies

  let check_fleets (fleets, armies) =
    List.iter
      (fun (s,t,su) ->
         if not (SU.exists (fun du -> match du.order with AMove (u,p) -> (stand_on u) = s && p = t | _ -> assert false) su)
         then SU.iter (fun du -> set_mark du Void) su)
      fleets

  let check_armies (fleets, armies) dc =
    SU.fold
      (fun du dc -> match du.order with
           AMove (u,p) -> begin
             if List.exists (fun (s,t,_) -> (stand_on u) = s && p = t) fleets
             then du::dc
             else (set_mark du NoConvoy; dc)
           end
         | _ -> assert false)
      armies dc

  let mark_all_invalid_convoy_orders su =
    let orders = get_convoy_orders su in
    check_fleets orders;
    check_armies orders []

end

let mark_all_invalid_convoy_orders (su: SU.t) : data_convoy =
  Step1.mark_all_invalid_convoy_orders su

(* -------------------------------------------------------------------------- *)
