open MapBoard

type utag = [`Armies|`Fleets]
type otag = [`Hold|`Move|`SupportA|`SupportD|`Convoy]

type order =
  | AHold of armies units
  | AMove of armies units * armies province
  | ASupportD of armies units * armies province
  | ASupportA of armies units * armies province * armies province
  | FHold of fleets units
  | FMove of fleets units * fleets province
  | FSupportD of fleets units * fleets province
  | FSupportA of fleets units * fleets province * fleets province
  | FConvoy of fleets units * armies province * armies province

let error_province = Invalid_argument "incompatible province type"
let error_units = Invalid_argument "incompatible unit type"
let error_number = Invalid_argument "wrong number of argument"
let error_order = Invalid_argument "order mismatch"

let is_convoying = function
  | AMove (u, p) -> not (is_accessible u p)
  | _ -> false

let units = function
  | AHold a | AMove (a,_) | ASupportD (a,_)
  | ASupportA (a,_,_) -> generalize a
  | FHold f | FMove (f,_) |FSupportD (f,_)
  | FSupportA (f,_,_) | FConvoy (f,_,_) -> generalize f

let ahold = function AHold a -> a | _ -> raise error_order
let amove = function AMove (a,p) -> (a,p) | _ -> raise error_order
let asupportd = function ASupportD (a,p) -> (a,p) | _ -> raise error_order
let asupporta = function ASupportA (a,p1,p2) -> (a,p1,p2) | _ -> raise error_order
let fhold = function FHold f -> f | _ -> raise error_order
let fmove = function FMove (f,p) -> (f,p) | _ -> raise error_order
let fsupportd = function FSupportD (f,p) -> (f,p) | _ -> raise error_order
let fsupporta = function FSupportA (f,p1,p2) -> (f,p1,p2) | _ -> raise error_order
let fconvoy = function FConvoy (f,p1,p2) -> (f,p1,p2) | _ -> raise error_order

let is_ahold = function AHold _ -> true | _ -> false
let is_amove = function AMove _ -> true | _ -> false
let is_asupportd = function ASupportD _ -> true | _ -> false
let is_asupporta = function ASupportA _ -> true | _ -> false
let is_fhold = function FHold _ -> true | _ -> false
let is_fmove = function FMove _ -> true | _ -> false
let is_fsupportd = function FSupportD _ -> true | _ -> false
let is_fsupporta = function FSupportA _ -> true | _ -> false
let is_fconvoy = function FConvoy _ -> true | _ -> false


let utoa (u: any units) : armies units =
  match specialize u with
  | Some u, None -> u
  | _ -> raise error_units
let utof (u: any units) : fleets units =
  match specialize u with
  | None, Some u -> u
  | _ -> raise error_units

let ptoa (p: any province) : armies province =
  match from_any p with
  | Some p, None, None -> inland_to_armies p
  | None, None, Some p -> coastal_to_armies p
  | _ -> raise error_province

let ptof (p: any province) : fleets province =
  match from_any p with
  | None, Some p, None -> water_to_fleets p
  | None, None, Some p -> coastal_to_fleets p
  | _ -> raise error_province


let search (str: string) (t: t) =
  try search_by_name str t with
    Not_found -> search_by_abbr str t

let list1 (t: t) = function
    [a] -> search a t
  | _ -> raise error_number
let list2 t = function
    [a;b] -> (search a t), (search b t)
  | _ -> raise error_number
let list3 t = function
    [a;b;c] -> (search a t), (search b t), (search c t)
  | _ -> raise error_number


let on_province t p =
  match on_province t p with
  | Some u -> u
  | None -> raise error_order


let tags = function
  | AHold _ -> `Armies, `Hold
  | AMove _ -> `Armies, `Move
  | ASupportD _ -> `Armies, `SupportD
  | ASupportA _ -> `Armies, `SupportA
  | FHold _ -> `Fleets, `Hold
  | FMove _ -> `Fleets, `Move
  | FSupportD _ -> `Fleets, `SupportD
  | FSupportA _ -> `Fleets, `SupportA
  | FConvoy _ -> `Fleets, `Convoy

let make (t: t) (u: [< utag]) (o: [< otag]) (lst: string list) =
  match u with
  | `Armies -> begin
      match o with
      | `Hold ->
        let p1 = list1 t lst in
        AHold (utoa (on_province t p1))
      | `Move ->
        let (p1,p2) = list2 t lst in
        AMove (utoa (on_province t p1), ptoa p2)
      | `SupportD ->
        let (p1,p2) = list2 t lst in
        ASupportD (utoa (on_province t p1), ptoa p2)
      | `SupportA ->
        let (p1,p2,p3) = list3 t lst in
        ASupportA (utoa (on_province t p1), ptoa p2, ptoa p3)
      | `Convoy -> raise error_units
    end
  | `Fleets -> begin
      match o with
      | `Hold ->
        let p1 = list1 t lst in
        FHold (utof (on_province t p1))
      | `Move ->
        let (p1,p2) = list2 t lst in
        FMove (utof (on_province t p1), ptof p2)
      | `SupportD ->
        let (p1,p2) = list2 t lst in
        FSupportD (utof (on_province t p1), ptof p2)
      | `SupportA ->
        let (p1,p2,p3) = list3 t lst in
        FSupportA (utof (on_province t p1), ptof p2, ptof p3)
      | `Convoy ->
        let (p1,p2,p3) = list3 t lst in
        FConvoy (utof (on_province t p1), ptoa p2, ptoa p3)
    end
