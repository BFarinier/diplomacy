type country =
    Austria | England | France | Germany
  | Italy | Russia | Turkey | Neutral

type province_sort = Inland | Water | Coastal

type province = {
  sort : province_sort;
  name : string;
  abbr : string;
  supply : bool;
  adj_armies : province list;
  adj_fleets : province list;
  mutable country : country
}

module BS = Set.Make(struct
    type t = province
    let compare t1 t2 =
      compare
        (t1.sort, t1.supply, t1.abbr, t1.name)
        (t2.sort, t2.supply, t2.abbr, t2.name)
  end)
module BM = Map.Make(struct
    type t = province
    let compare t1 t2 =
      compare
        (t1.sort, t1.supply, t1.abbr, t1.name)
        (t2.sort, t2.supply, t2.abbr, t2.name)
  end)

type 'a t = (BS.t * 'a BM.t)

type units_sort = Armies | Fleets

type units = {
  sort : units_sort;
  country : country;
  mutable province : province
}


let provinces (bs,_: 'a t) : province list =
  BS.elements bs

let supply_centers (bs,_: 'a t) : province list =
  BS.elements (BS.filter (fun p -> p.supply) bs)

let provinces_controlled_by (c: country) (bs,_ : 'a t) : province list =
  BS.elements (BS.filter (fun p -> p.country = c) bs)


let controlled_by (p: province) : country = p.country
let is_supply_center (p: province) : bool = p.supply

let own_by (u: units) : country = u.country
let stand_on (u: units) : province = u.province


let on_province ((_,bm): 'a t) (p: province) : 'a =
  BM.find p bm

let on_mapboard ((_,bm): 'a t) : (province * 'a) list =
  BM.bindings bm


let add_unit a (bs,bm) = bs, BM.add a.province a bm
let remove_unit a (bs,bm) = bs, BM.remove a.province bm
let find_unit a (bs,bm) = bs, BM.find a.province bm


let are_adjacent_for (u: units) (p1: province) (p2: province) : bool =
  let l1, l2 =
    (match u.sort with
     | Armies -> p1.adj_armies, p2.adj_armies
     | Fleets -> p1.adj_fleets, p2.adj_fleets)
  in
  let b = List.mem p1 l2 in
  assert (List.mem p2 l1 = b); b

let move_unit (u:units) (p: province) (b: units t) : units t =
  assert (are_adjacent_for u u.province p);
  u.province <- p;
  add_unit u b



let rec den : province = {
  sort = Coastal;
  name = "Denmark";
  abbr = "DEN";
  supply = false;
  adj_armies = [den;spa];
  adj_fleets = [den;bre];
  country = Neutral }
and bre : province = {
  sort = Coastal;
  name = "Brest";
  abbr = "BRE";
  supply = false;
  adj_armies = [den;spa];
  adj_fleets = [den;spa];
  country = Neutral }
and spa : province = {
  sort = Coastal;
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
