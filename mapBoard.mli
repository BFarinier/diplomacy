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
val from_any : any province ->
  inland province option * water province option * coastal province option

val inland_to_armies : inland province -> armies province
val coastal_to_armies : coastal province -> armies province
val coastal_to_fleets : coastal province -> fleets province
val water_to_fleets : water province -> fleets province

val get_name_abbr : 'a province -> string * string
val homeland_of : armies province -> country
val controlled_by : armies province -> country
val is_supply_center : armies province -> bool


type 'a units

val generalize : 'a units -> any units
val specialize : any units -> armies units option * fleets units option

val own_by : 'a units -> country
val stand_on : 'a units -> 'a province
val take_control : armies units -> unit


type t
val boardmap : t
val next_season : t -> t

val provinces : t -> any province list
val supply_centers : t -> any province list
val on_province : t -> any province -> any units option
val on_mapboard : t -> (any province * any units) list
val controlled_by : country -> t -> any province list
val search_by_name : string -> t -> any province
val search_by_abbr : string -> t -> any province

val is_accessible : 'a units -> 'a province -> bool
val are_adjacent : 'a province -> 'a province -> bool

val move_armies : armies units -> armies province -> t -> t
val move_fleets : fleets units -> fleets province -> t -> t
val create_armies : country -> armies province -> t -> armies units
val create_fleets : country -> fleets province -> t -> fleets units
