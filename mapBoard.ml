type country =
    Austria | England | France | Germany
  | Italy | Russia | Turkey | Neutral

type any = [`Inland|`Water|`Coastal]

type armies = [`Inland|`Coastal]
type fleets = [`Water|`Coastal]

type 'a province = {
  sort : 'a;
  name : string;
  abbr : string;
  supply : bool;
  adj_armies : armies province list;
  adj_fleets : fleets province list;
  mutable country : country
}

module BS = Set.Make(struct
    type t = any province
    let compare t1 t2 =
      compare
        (t1.sort, t1.supply, t1.abbr, t1.name)
        (t2.sort, t2.supply, t2.abbr, t2.name)
  end)

module BM = Map.Make(struct
    type t = any province
    let compare t1 t2 =
      compare
        (t1.sort, t1.supply, t1.abbr, t1.name)
        (t2.sort, t2.supply, t2.abbr, t2.name)
  end)

type 'a t = (BS.t * 'a BM.t)

type 'a u = {
  sort : 'a;
  country : country;
  mutable province : 'a province
}

type _ units =
  | Armies : armies u -> armies units
  | Fleets : fleets u -> fleets units

let provinces (bs,_: 'a t) : any province list =
  BS.elements bs

let supply_centers (bs,_: 'a t) : any province list =
  BS.elements (BS.filter (fun p -> p.supply) bs)

let provinces_controlled_by (c: country) (bs,_ : 'a t) : any province list =
  BS.elements (BS.filter (fun p -> p.country = c) bs)


let controlled_by (p: _ province) : country = p.country
let is_supply_center (p: _ province) : bool = p.supply

let unpack (type a) (u: a units) : a u =
  match u with
  | Armies u -> u
  | Fleets u -> u

let update (type a) (u: a units) (p: a province) : unit =
  match u with
  | Armies u -> u.province <- p
  | Fleets u -> u.province <- p

let own_by (type a) (u: a units) : country = (unpack u).country
let stand_on (type a) (u: a units) : a province = (unpack u).province


let on_province ((_,bm): 'a t) (p: any province) : 'a =
  BM.find p bm

let on_mapboard ((_,bm): 'a t) : (any province * 'a) list =
  BM.bindings bm


let add_unit (u: any units) (bs,bm) =  bs, BM.add (stand_on u) u bm
let remove_unit (u: any units) (bs,bm) = bs, BM.remove (stand_on u) bm
let find_unit (u: any units) (bs,bm) = bs, BM.find (stand_on u) bm


let check_adj p1 p2 l1 l2 =
  let b = List.mem p1 l2 in
  assert (List.mem p2 l1 = b); b


let are_adjacent_for (type a) (u: a units) (p1: a province) (p2: a province) : bool =
  match u with
  | Armies _ -> check_adj p1 p2 p1.adj_armies p2.adj_armies
  | Fleets _ -> check_adj p1 p2 p1.adj_fleets p2.adj_fleets

let move_unit (u: any units) (p: any province) (b: any units t) : any units t =
  assert (are_adjacent_for u (stand_on u) p);
  update u p;
  add_unit u b



let rec den : 'a. ([>`Coastal] as 'a) province = {
  sort = `Coastal;
  name = "Denmark";
  abbr = "DEN";
  supply = false;
  adj_armies = [den;spa];
  adj_fleets = [den;bre];
  country = Neutral }
and bre : 'a. ([>`Coastal] as 'a)  province = {
  sort = `Coastal;
  name = "Brest";
  abbr = "BRE";
  supply = false;
  adj_armies = [den;spa];
  adj_fleets = [den;spa];
  country = Neutral }
and spa : 'a. ([>`Coastal] as 'a)  province = {
  sort = `Coastal;
  name = "Spain";
  abbr = "SPA";
  supply = false;
  adj_armies = [den;bre];
  adj_fleets = [den;bre];
  country = Neutral }
let boardmap : unit t =
  BS.empty, List.fold_left (fun bm p -> BM.add p () bm) BM.empty [den; bre; spa]

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
