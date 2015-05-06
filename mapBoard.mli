type country =
    Austria | England | France | Germany
  | Italy | Russia | Turkey | Neutral

type inland
type water
type coastal

type any
type armies
type fleets


type 'a province

val to_any : 'a province -> any province

val inland_to_armies : inland province -> armies province
val coastal_to_armies : coastal province -> armies province
val coastal_to_fleets : coastal province -> fleets province
val water_to_fleets : water province -> fleets province

val controlled_by : armies province -> country
val is_supply_center : armies province -> bool


type 'a units

val generalize : 'a units -> any units
val specialize : any units -> armies units option * fleets units option

val create_armies : country -> armies province -> armies units
val create_fleets : country -> fleets province -> fleets units

val own_by : 'a units -> country
val stand_on : 'a units -> 'a province


type t
val boardmap : t

val provinces : t -> any province list
val supply_centers : t -> any province list
val provinces_controlled_by : country -> t -> any province list
val on_province : t -> any province -> any units
val on_mapboard : t -> (any province * any units) list

val is_accessible : 'a units -> 'a province -> bool
val are_adjacent : 'a province -> 'a province -> bool

val move_armies : armies units -> armies province -> t -> t
val move_fleets : fleets units -> fleets province -> t -> t
