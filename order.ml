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



let is_convoying = function
  | AMove (u, p) -> is_accessible u p
  | _ -> false



let error_province = "incompatible province type"
let error_units = "incompatible unit type"
let error_number = "wrong number of argument"


let utoa (u: any units) : armies units =
  match specialize u with
  | Some u, None -> u
  | _ -> raise (Invalid_argument error_units)
let utof (u: any units) : fleets units =
  match specialize u with
  | None, Some u -> u
  | _ -> raise (Invalid_argument error_units)

let ptoa (p: any province) : armies province =
  match from_any p with
  | Some p, None, None -> inland_to_armies p
  | None, None, Some p -> coastal_to_armies p
  | _ -> raise (Invalid_argument error_province)

let ptof (p: any province) : fleets province =
  match from_any p with
  | None, Some p, None -> water_to_fleets p
  | None, None, Some p -> coastal_to_fleets p
  | _ -> raise (Invalid_argument error_province)


let search (str: string) (t: t) =
  try search_by_name str t with
    Not_found -> search_by_abbr str t

let list1 (t: t) = function
    [a] -> search a t
  | _ -> raise (Invalid_argument error_number)
let list2 t = function
    [a;b] -> (search a t), (search b t)
  | _ -> raise (Invalid_argument error_number)
let list3 t = function
    [a;b;c] -> (search a t), (search b t), (search c t)
  | _ -> raise (Invalid_argument error_number)


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
      | `Convoy -> raise (Invalid_argument error_units)
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
