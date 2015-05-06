type country =
    Austria | England | France | Germany
  | Italy | Russia | Turkey | Neutral

type any

type inland
type water
type coastal

type armies
type fleets


module P = struct

  type s = Inland | Water | Coastal

  type t = {
    sort : s;
    name : string;
    abbr : string;
    supply : bool;
    adj_armies : t list;
    adj_fleets : t list;
    mutable country : country
  }

  type 'a province = t
end

type 'a province = 'a P.province

let to_any (p:'a province) : any province = p

let inland_to_armies  (p: inland  province) : armies province = p
let coastal_to_armies (p: coastal province) : armies province = p
let coastal_to_fleets (p: coastal province) : fleets province = p
let water_to_fleets   (p: water   province) : fleets province = p

let controlled_by (p: any province) : country = p.P.country
let is_supply_center (p: any province) : bool = p.P.supply


module U = struct

  type 'a t = {
    country : country;
    mutable province : 'a province
  }

  type _ units =
    | Armies : armies t -> armies units 
    | Fleets : fleets t -> fleets units
    | Any_armies : armies t -> any units
    | Any_fleets : fleets t -> any units

  let upcast (type a) (u: a units) : any units =
    match u with
    | Armies u -> Any_armies u
    | Fleets u -> Any_fleets u
    | Any_armies _ -> u
    | Any_fleets _ -> u

  let downcast (type a) (u: a units) : armies units option * fleets units option =
    match u with
    | Armies _ -> Some u, None
    | Fleets _ -> None, Some u
    | Any_armies u -> Some (Armies u), None
    | Any_fleets u -> None, Some (Fleets u)

  let update (type a) (u : a units) (p : a province) =
    match u with
    | Armies u -> u.province <- p
    | Fleets u -> u.province <- p
    | Any_armies u -> u.province <- p
    | Any_fleets u -> u.province <- p

end

type 'a units = 'a U.units

let create_armies (c: country) (p: armies province) : armies units =
  U.(Armies { country = c; province = p })
let create_fleets (c: country) (p: fleets province) : fleets units =
  U.(Fleets { country = c; province = p })

let own_by (u: any units) : country =
  U.(match u with
      | Any_armies u -> u.country
      | Any_fleets u -> u.country)

let rec stand_on (type a) (u: a units) : a province =
  U.(match u with
      | Armies u -> u.province
      | Fleets u -> u.province
      | Any_armies u -> u.province
      | Any_fleets u -> u.province)


module BS = Set.Make(struct
    type t = any province
    let compare t1 t2 =
      compare
        P.(t1.sort, t1.supply, t1.abbr, t1.name)
        P.(t2.sort, t2.supply, t2.abbr, t2.name)
  end)

module BM = Map.Make(struct
    type t = any province
    let compare t1 t2 =
      compare
        P.(t1.sort, t1.supply, t1.abbr, t1.name)
        P.(t2.sort, t2.supply, t2.abbr, t2.name)
  end)

type 'a t = (BS.t * 'a BM.t)


let provinces (bs,_: 'a t) : any province list =
  BS.elements bs

let supply_centers (bs,_: 'a t) : any province list =
  BS.elements (BS.filter (fun p -> p.P.supply) bs)

let provinces_controlled_by (c: country) (bs,_ : 'a t) : any province list =
  BS.elements (BS.filter (fun p -> p.P.country = c) bs)

let on_province ((_,bm): 'a t) (p: any province) : 'a =
  BM.find p bm

let on_mapboard ((_,bm): 'a t) : (any province * 'a) list =
  BM.bindings bm


let add_unit (u: any units) (bs,bm) =  bs, BM.add (stand_on u) u bm
let remove_unit (u: any units) (bs,bm) = bs, BM.remove (stand_on u) bm


let check_adj p1 p2 l1 l2 =
  let b = List.mem p1 l2 in
  assert (List.mem p2 l1 = b); b

let are_adjacent (type a) (u: a units) (p: a province) : bool =
  U.(match u with
      | Armies u -> check_adj p u.province p.P.adj_armies u.province.P.adj_armies
      | Fleets u -> check_adj p u.province p.P.adj_fleets u.province.P.adj_fleets
      | Any_armies u -> check_adj p u.province p.P.adj_armies u.province.P.adj_armies
      | Any_fleets u -> check_adj p u.province p.P.adj_fleets u.province.P.adj_fleets)

let move_armies (u: armies units) (p: armies province) (b: any units t) : any units t =
  assert (are_adjacent u p);
  U.update u p;
  add_unit (U.upcast u) b

let move_fleets (u: fleets units) (p: fleets province) (b: any units t) : any units t =
  assert (are_adjacent u p);
  U.update u p;
  add_unit (U.upcast u) b


let rec den : coastal province = P.{
    sort = Coastal;
    name = "Denmark";
    abbr = "DEN";
    supply = false;
    adj_armies = [bre;spa;par];
    adj_fleets = [bre;spa;cha];
    country = Neutral }
and bre : coastal province = P.{
    sort = Coastal;
    name = "Brest";
    abbr = "BRE";
    supply = false;
    adj_armies = [den;spa;par];
    adj_fleets = [den;spa;cha];
    country = Neutral }
and spa : coastal province = P.{
    sort = Coastal;
    name = "Spain";
    abbr = "SPA";
    supply = false;
    adj_armies = [den;bre;par];
    adj_fleets = [den;bre;cha];
    country = Neutral }
and par : inland province = P.{
    sort = Inland;
    name = "Paris";
    abbr = "PAR";
    supply = false;
    adj_armies = [bre;den;spa];
    adj_fleets = [bre;den;spa;cha];
    country = Neutral }
and cha : water province = P.{
    sort = Water;
    name = "Channel";
    abbr = "CHA";
    supply = false;
    adj_armies = [den;bre;spa;par];
    adj_fleets = [den;bre;spa];
    country = Neutral }
let boardmap : unit t =
  BS.empty, List.fold_left (fun bm p -> BM.add p () bm) BM.empty [den; bre; spa; par; cha]

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
