open MapBoard

type order =
  | AHold of armies units
  | AMove of armies units * armies province
  | ASupportD of armies units * armies units
  | ASupportA of armies units * armies units * armies province
  | FHold of fleets units
  | FMove of fleets units * fleets province
  | FSupportD of fleets units * fleets units
  | FSupportA of fleets units * fleets units * fleets province
  | FConvoy of fleets units * armies province * armies province

let utoa (u: any units) : armies units =
  match specialize u with
  | Some u, None -> u
  | _ -> raise (Invalid_argument "units_to_armies")
let utof (u: any units) : fleets units =
  match specialize u with
  | None, Some u -> u
  | _ -> raise (Invalid_argument "units_to_fleets")

let ptoa (p: any province) : armies province =
  match from_any p with
  | Some p, None, None -> inland_to_armies p
  | None, None, Some p -> coastal_to_armies p
  | _ -> raise (Invalid_argument "province_to_armies")

let ptof (p: any province) : fleets province =
  match from_any p with
  | None, Some p, None -> water_to_fleets p
  | None, None, Some p -> coastal_to_fleets p
  | _ -> raise (Invalid_argument "province_to_fleets")

let search str t =
  try search_by_name str t with
    Not_found -> search_by_abbr str t

let list1 t = function
    [a] -> search a t
  | _ -> raise (Invalid_argument "list1")
let list2 t = function
    [a;b] -> (search a t), (search b t)
  | _ -> raise (Invalid_argument "list2")
let list3 t = function
    [a;b;c] -> (search a t), (search b t), (search c t)
  | _ -> raise (Invalid_argument "list2")

let make u t (b: t) (lst: string list) =
  match u with
  | `Armies -> begin
      match t with
      | `Hold ->
        let p1 = list1 b lst in
        AHold (utoa (on_province b p1))
      | `Move ->
        let (p1,p2) = list2 b lst in
        AMove (utoa (on_province b p1), ptoa p2)
      | `SupportD ->
        let (p1,p2) = list2 b lst in
        ASupportD (utoa (on_province b p1), utoa (on_province b p2))
      | `SupportA ->
        let (p1,p2,p3) = list3 b lst in
        ASupportA (utoa (on_province b p1), utoa (on_province b p2), ptoa p3)
      | `Convoy -> raise (Invalid_argument "make_order")
    end
  | `Fleets -> begin
      match t with
      | `Hold ->
        let p1 = list1 b lst in
        FHold (utof (on_province b p1))
      | `Move ->
        let (p1,p2) = list2 b lst in
        FMove (utof (on_province b p1), ptof p2)
      | `SupportD ->
        let (p1,p2) = list2 b lst in
        FSupportD (utof (on_province b p1), utof (on_province b p2))
      | `SupportA ->
        let (p1,p2,p3) = list3 b lst in
        FSupportA (utof (on_province b p1), utof (on_province b p2), ptof p3)
      | `Convoy ->
        let (p1,p2,p3) = list3 b lst in
        FConvoy (utof (on_province b p1), ptoa p2, ptoa p3)
    end
