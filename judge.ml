(* http://www.floc.net/dpjudge/?page=Algorithm *)

open Prelude
open MapBoard
open Order

type mark =
    Clear | Stuck
  | ConvoyEndangered | ConvoyUnderAttack
  | Bounce | Cut | Dislodged

type data_units = {
  order : order;
  mutable mark : mark;
  mutable count : int;
  mutable nohelp : any units list;
}

let order du = du.order
let mark du = du.mark
let count du = du.count
let nohelp du = du.nohelp

let clear du = du.mark <- Clear
let stuck du = du.mark <- Stuck
let endangered du = du.mark <- ConvoyEndangered
let underattack du = du.mark <- ConvoyUnderAttack
let bounce du = du.mark <- Bounce
let cut du = du.mark <- Cut
let dislodged du = du.mark <- Dislodged

let is_clear du = du.mark = Clear
let is_stuck du = du.mark = Stuck
let is_endangered du = du.mark = ConvoyEndangered
let is_underattack du = du.mark = ConvoyUnderAttack
let is_bounce du = du.mark = Bounce
let is_cut du = du.mark = Cut
let is_dislodged du = du.mark = Dislodged

let add_nohelp du u = du.nohelp <- u :: du.nohelp
let incr du = du.count <- succ du.count
let decr du = du.count <- pred du.count

type data_province = {
  province : any province;
  mutable combats : order list;
}

let province dp = dp.province
let combats dp = dp.combats
let set_combats dp c = dp.combats <- c

type data_convoy = data_units list

module SU = Set.Make(struct
    type t = data_units
    let compare d1 d2 = compare d1.order d2.order
  end)
module SP = Set.Make(struct
    type t = data_province
    let compare p1 p2 = compare p1.province p2.province
  end)

let exists_order f su = SU.exists (fun du -> is_clear du && f du.order) su
let filter_order f su = SU.filter (fun du -> is_clear du && f du.order) su
let partition_order f su = SU.partition (fun du -> is_clear du && f du.order) su

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
    if not b then SU.iter stuck conveyors; b



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
         then SU.iter stuck su)
      fleets

  let check_armies (fleets, armies) =
    SU.fold
      (fun du dc ->
         let (u,p) = amove (order du) in
         if List.exists (fun (s,t,su) -> (stand_on u) = s && p = t && check_convoy s t su) fleets
         then du::dc
         else (stuck du; dc))
      armies []



  let mark_all_invalid_convoy_orders su =
    let orders = get_convoy_orders su in
    let dc = check_armies orders in
    check_fleets orders; dc

end

let mark_all_invalid_convoy_orders (su: SU.t) : data_convoy =
  Step1.mark_all_invalid_convoy_orders su

(* -------------------------------------------------------------------------- *)

module Step2 = struct

  let extract_orders f g su =
    List.map (order %> f) (SU.elements (filter_order g su))

  let extract_moves su =
    (extract_orders amove is_amove su
     |> List.map (fun (u,p) -> generalize u, to_any p))
    @ (extract_orders fmove is_fmove su
       |> List.map (fun (u,p) -> generalize u, to_any p))

  let exists_attack u p1 p2 o t su =
    if (match on_province t (to_any p2) with
        | None -> false
        | Some u' -> own_by u = own_by u')
    then add_nohelp o (generalize u);
    match on_province t p2 with
    | None -> false
    | Some u ->
      extract_moves su
      |> List.exists (fun (u',p') -> u = u' && p' = p2)

  let valid_supporta t su o (u,p1,p2) =
    is_accessible u p2
    && on_province t (to_any p1) <> None
    && exists_attack u (to_any p1) (to_any p2) o t su

  let valid_supportd t o (u,p) =
    is_accessible u p && on_province t (to_any p) <> None

  let check_supports f (o,so) =
    if f o so then incr o else stuck o


  let extract_orders f g su =
    List.map (fun o -> o, f (order o)) (SU.elements (filter_order g su))

  let get_support_orders su =
    extract_orders asupportd is_asupportd su,
    extract_orders asupporta is_asupporta su,
    extract_orders fsupportd is_fsupportd su,
    extract_orders fsupporta is_fsupporta su

  let mark_all_invalid_support_orders t su =
    let (ad,aa,fd,fa) = get_support_orders su in
    List.iter (check_supports (valid_supportd t)) ad;
    List.iter (check_supports (valid_supporta t su)) aa;
    List.iter (check_supports (valid_supportd t)) fd;
    List.iter (check_supports (valid_supporta t su)) fa

end

let mark_all_invalid_support_orders (t: MapBoard.t) (su: SU.t) : unit =
  Step2.mark_all_invalid_support_orders t su
