type country =
    Austria | England | France | Germany
  | Italy | Russia | Turkey | Neutral

type inland
type water
type coastal

type any
type armies
type fleets



type ps = Inland | Water | Coastal

type pt = {
  psort : ps;
  name : string;
  abbr : string;
  supply : bool;
  homeland : country;
  mutable country : country;
  mutable adj_armies : pt list;
  mutable adj_fleets : pt list;
}

type 'a province = pt

let to_any (p:'a province) : any province = p
let from_any (p: any province) :
  inland province option  * water province option * coastal province option =
  match p.psort with
  | Inland -> Some p, None, None
  | Water -> None, Some p, None
  | Coastal -> None, None, Some p

let inland_to_armies  (p: inland  province) : armies province = p
let coastal_to_armies (p: coastal province) : armies province = p
let coastal_to_fleets (p: coastal province) : fleets province = p
let water_to_fleets   (p: water   province) : fleets province = p

let get_name_abbr (p: 'a province) : string * string = p.name, p.abbr
let homeland_of (p: armies province) : country = p.homeland
let controlled_by (p: armies province) : country = p.country
let is_supply_center (p: armies province) : bool = p.supply



type us = Armies | Fleets

type ut = {
  usort : us;
  country : country;
  mutable province : pt
}

type 'a units = ut

let generalize (u: 'a units) : any units = u
let specialize (u: any units) : armies units option * fleets units option =
  match u.usort with
  | Armies -> Some u, None
  | Fleets -> None, Some u

let own_by (u: 'a units) : country = u.country
let stand_on (u: 'a units) : 'a province = u.province
let take_control (u: armies units) = u.province.country <- u.country



module BS = Set.Make(struct
    type t = any province
    let compare t1 t2 =
      compare
        (t1.psort, t1.supply, t1.abbr, t1.name)
        (t2.psort, t2.supply, t2.abbr, t2.name)
  end)

module BM = Map.Make(struct
    type t = any province
    let compare t1 t2 =
      compare
        (t1.psort, t1.supply, t1.abbr, t1.name)
        (t2.psort, t2.supply, t2.abbr, t2.name)
  end)

type t = (BS.t * any units BM.t)


let provinces (bs,_: t) : any province list =
  BS.elements bs

let supply_centers (bs,_: t) : any province list =
  BS.elements (BS.filter (fun p -> p.supply) bs)

let controlled_by (c: country) (bs,_ : t) : any province list =
  BS.elements (BS.filter (fun p -> p.country = c) bs)

let on_province ((_,bm): t) (p: any province) : 'a =
  BM.find p bm

let on_mapboard ((_,bm): t) : (any province * any units) list =
  BM.bindings bm


let add_unit (u: any units) (bs,bm) =  bs, BM.add (stand_on u) u bm
let remove_unit (u: any units) (bs,bm) = bs, BM.remove (stand_on u) bm

let search_by_name (name: string) (bs,_: t) : any province =
  let name = String.(lowercase @@ escaped @@ trim name) in
  BS.filter (fun p -> (String.lowercase p.name) = name) bs
  |> BS.elements
  |> function | [p] -> p | _ -> raise Not_found

let search_by_abbr (abbr: string) (bs,_: t) : any province =
  let abbr = String.(lowercase @@ escaped @@ trim abbr) in
  BS.filter (fun p -> (String.lowercase p.abbr) = abbr) bs
  |> BS.elements
  |> function | [p] -> p | _ -> raise Not_found



let check_adj p1 p2 l1 l2 =
  let b = List.mem p1 l2 in
  assert (List.mem p2 l1 = b); b

let is_accessible (u: 'a units) (p: 'a province) : bool =
  match u.usort with
  | Armies -> check_adj p u.province p.adj_armies u.province.adj_armies
  | Fleets -> check_adj p u.province p.adj_fleets u.province.adj_fleets

let are_adjacent (p1: 'a province) (p2: 'a province) : bool =
  check_adj p1 p2 p1.adj_armies p2.adj_armies
  || check_adj p1 p2 p1.adj_fleets p2.adj_fleets


let move_armies (u: armies units) (p: armies province) (b: t) : t =
  assert (is_accessible u p);
  u.province <- p;
  add_unit (generalize u) b

let move_fleets (u: fleets units) (p: fleets province) (b: t) : t =
  assert (is_accessible u p);
  u.province <- p;
  add_unit (generalize u) b


let create_armies (c: country) (p: armies province) (_,bm: t) : armies units =
  assert (p.country = c && p.supply && not (BM.mem p bm));
  { usort = Armies; country = c; province = p }
let create_fleets (c: country) (p: fleets province) (_,bm: t): fleets units =
  assert (p.country = c && p.supply && not (BM.mem p bm));
  { usort = Fleets; country = c; province = p }


let next_season (bs,bm: t) : t = (bs, BM.empty)


let rec den : coastal province = {
  psort = Coastal;
  name = "Denmark";
  abbr = "DEN";
  supply = false;
  adj_armies = [bre;spa;par];
  adj_fleets = [bre;spa;cha];
  homeland = Neutral;
  country = Neutral }
and bre : coastal province = {
  psort = Coastal;
  name = "Brest";
  abbr = "BRE";
  supply = false;
  adj_armies = [den;spa;par];
  adj_fleets = [den;spa;cha];
  homeland = Neutral;
  country = Neutral }
and spa : coastal province = {
  psort = Coastal;
  name = "Spain";
  abbr = "SPA";
  supply = false;
  adj_armies = [den;bre;par];
  adj_fleets = [den;bre;cha];
  homeland = Neutral;
  country = Neutral }
and par : inland province = {
  psort = Inland;
  name = "Paris";
  abbr = "PAR";
  supply = false;
  adj_armies = [bre;den;spa];
  adj_fleets = [bre;den;spa;cha];
  homeland = Neutral;
  country = Neutral }
and cha : water province = {
  psort = Water;
  name = "Channel";
  abbr = "CHA";
  supply = false;
  adj_armies = [den;bre;spa;par];
  adj_fleets = [den;bre;spa];
  homeland = Neutral;
  country = Neutral }
let boardmap : t =
  BS.empty, List.fold_left (fun bm p -> BM.add p (Obj.magic ()) bm) BM.empty [den; bre; spa; par; cha]

let autria =
  Austria,
  [
    "Bohemia","boh";
    "Budapest","bud";
    "Galicia","gal";
    "Trieste","tri";
    "Tyrolia","tyr";
    "Vienna","vie"
  ]
let england =
  England,
  [
    "Clyde","cly";
    "Edinburgh","edi";
    "Liverpool","lvp";
    "London","lon";
    "Wales","wal";
    "Yorkshire","yor"
  ]
let france =
  France,
  [
    "Brest","bre";
    "Burgundy","bur";
    "Gascony","gas";
    "Marseilles","mar";
    "Paris","par";
    "Picardy","pic"
  ]
let germany =
  Germany,
  [
    "Berlin","ber";
    "Kiel","kie";
    "Munich","mun";
    "Prussia","pru";
    "Ruhr","ruh";
    "Silesia","sil"
  ]
let italy =
  Italy,
  [
    "Apulia","apu";
    "Naples","nap";
    "Piedmont","pie";
    "Rome","rom";
    "Tuscany","tus";
    "Venice","ven"
  ]
let russia =
  Russia,
  [
    "Livonia","lvn";
    "Moscow","mos";
    "Sevastopol","sev";
    "St Petersburg","stp";
    "Ukraine","ukr";
    "Warsaw","war"
  ]
let turkey =
  Turkey,
  [
    "Ankara","ank";
    "Armenia","arm";
    "Constantinople","con";
    "Smyrna","smy";
    "Syria","syr"
  ]
let neutral =
  Neutral,
  [
    "Albania","alb";
    "Belgium","bel";
    "Bulgaria","bul";
    "Denmark","den";
    "Finland","fin";
    "Greece","gre";
    "Holland","hol";
    "Norway","nwy";
    "North Africa","naf";
    "Portugal","por";
    "Rumania","rum";
    "Serbia","ser";
    "Spain","spa";
    "Sweden","swe";
    "Tunis","tun"
  ]
let sea =
  Neutral,
  [
    "Adriatic Sea","adr";
    "Aegean Sea","aeg";
    "Baltic Sea","bal";
    "Barents Sea","bar";
    "Black Sea","bla";
    "Eastern Mediterranean","eas";
    "English Channel","eng";
    "Gulf of Bothnia","gob";
    "Gulf of Lyon","gol";
    "Helgoland Bight","hel";
    "Ionian Sea","ion";
    "Irish Sea","iri";
    "Mid-Atlantic Ocean","mao";
    "North Atlantic Ocean","nao";
    "North Sea","nth";
    "Norwegian Sea","nwg";
    "Skagerrak","ska";
    "Tyrrhenian Sea","tys";
    "Western Mediterranean","wes"
  ]
