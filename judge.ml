(* http://www.floc.net/dpjudge/?page=Algorithm *)

open Prelude
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

let order du = du.order
let mark du = du.mark
let count du = du.count
let set_mark du m = du.mark <- m
let set_count du c = du.count <- c

type data_province = {
  province : any province;
  mutable combats : order list;
}

let province dp = dp.province
let combats dp = dp.combats
let set_combats dp c = dp.combats <- c

type data_convoy = data_units list

module SU = Set.Make(struct type t = data_units let compare = compare end)
module SP = Set.Make(struct type t = data_province let compare = compare end)

let exists_order f su = SU.exists (fun du -> du.mark = Nothing && f du.order) su
let filter_order f su = SU.filter (fun du -> du.mark = Nothing && f du.order) su
let partition_order f su = SU.partition (fun du -> du.mark = Nothing && f du.order) su

let ptof u = match from_any (to_any u) with
  | None, Some u, None -> water_to_fleets u
  | None, None, Some u -> coastal_to_fleets u
  | _ -> assert false
let ptoa u = match from_any (to_any u) with
  | Some u, None, None -> inland_to_armies u
  | None, None, Some u -> coastal_to_armies u
  | _ -> assert false

(* -------------------------------------------------------------------------- *)

module Step1 = struct

  let check_visited p visited not_visited =
    let b = not (List.mem p visited) in
    assert (List.mem p not_visited = b); b

  let update_visited p p' visited not_visited =
    p', p::visited, List.filter (fun p' -> not (p = p')) not_visited

  let next_fleets visited not_visited p =
    List.fold_left (fun acc p' ->
        if are_adjacent p p' && check_visited p' visited not_visited
        then (update_visited p p' visited not_visited)::acc
        else acc) [] not_visited

  let rec exists_chain source target visited not_visited =
    (are_adjacent source target)
    || (not_visited <> []
        && List.exists
          (fun (s, visited, not_visited) -> exists_chain s target visited not_visited)
          (next_fleets visited not_visited source))

  let check_convoy source target conveyors =
    let b =
      List.map
        (order %> fconvoy %> fun (u,_,_) -> ptof (stand_on u))
        (SU.elements conveyors)
      |> exists_chain (ptof source) (ptof target) []
    in
    if not b then SU.iter (fun du -> set_mark du Void) conveyors; b



  let rec split_convoy_order acc su =
    if SU.is_empty su then acc
    else
      let (_,s,t) = fconvoy (order (SU.choose su)) in
      let (su1,su2) = partition_order (fconvoy %> fun (_,s',t') -> s = s' && t = t') su in
      split_convoy_order ((s,t,su1)::acc) su2

  let get_convoy_orders su =
    let fleets =
      filter_order is_fconvoy su
      |> split_convoy_order [] in
    let armies = filter_order is_convoying su in
    fleets, armies



  let check_fleets (fleets, armies) =
    List.iter
      (fun (s,t,su) ->
         if not (exists_order (amove %> fun (u,p) -> (stand_on u) = s && p = t) su)
         then SU.iter (flip set_mark Void) su)
      fleets

  let check_armies (fleets, armies) =
    SU.fold
      (fun du dc ->
        let (u,p) = amove (order du) in
        if List.exists (fun (s,t,su) -> (stand_on u) = s && p = t && check_convoy s t su) fleets
        then du::dc
        else (set_mark du NoConvoy; dc))
      armies []



  let mark_all_invalid_convoy_orders su =
    let orders = get_convoy_orders su in
    let dc = check_armies orders in
    check_fleets orders; dc

end

let mark_all_invalid_convoy_orders (su: SU.t) : data_convoy =
  Step1.mark_all_invalid_convoy_orders su

(* -------------------------------------------------------------------------- *)
