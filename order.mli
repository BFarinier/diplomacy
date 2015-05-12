type utag = [ `Armies | `Fleets ]
type otag = [ `Convoy | `Hold | `Move | `SupportA | `SupportD ]
type order = private
    AHold of     MapBoard.armies MapBoard.units
  | AMove of     MapBoard.armies MapBoard.units * MapBoard.armies MapBoard.province
  | ASupportD of MapBoard.armies MapBoard.units * MapBoard.armies MapBoard.province
  | ASupportA of MapBoard.armies MapBoard.units * MapBoard.armies MapBoard.province * MapBoard.armies MapBoard.province
  | FHold of     MapBoard.fleets MapBoard.units
  | FMove of     MapBoard.fleets MapBoard.units * MapBoard.fleets MapBoard.province
  | FSupportD of MapBoard.fleets MapBoard.units * MapBoard.fleets MapBoard.province
  | FSupportA of MapBoard.fleets MapBoard.units * MapBoard.fleets MapBoard.province * MapBoard.fleets MapBoard.province
  | FConvoy of   MapBoard.fleets MapBoard.units * MapBoard.armies MapBoard.province * MapBoard.armies MapBoard.province

val is_convoying : order -> bool

val tags : order -> [> utag ] * [> otag ]
val make : MapBoard.t -> [< utag ] -> [< otag ] -> string list -> order

val units : order -> MapBoard.any MapBoard.units

val ahold :     order -> MapBoard.armies MapBoard.units
val amove :     order -> MapBoard.armies MapBoard.units * MapBoard.armies MapBoard.province
val asupportd : order -> MapBoard.armies MapBoard.units * MapBoard.armies MapBoard.province
val asupporta : order -> MapBoard.armies MapBoard.units * MapBoard.armies MapBoard.province * MapBoard.armies MapBoard.province
val fhold :     order -> MapBoard.fleets MapBoard.units
val fmove :     order -> MapBoard.fleets MapBoard.units * MapBoard.fleets MapBoard.province
val fsupportd : order -> MapBoard.fleets MapBoard.units * MapBoard.fleets MapBoard.province
val fsupporta : order -> MapBoard.fleets MapBoard.units * MapBoard.fleets MapBoard.province * MapBoard.fleets MapBoard.province
val fconvoy :   order -> MapBoard.fleets MapBoard.units * MapBoard.armies MapBoard.province * MapBoard.armies MapBoard.province

val is_ahold :     order -> bool
val is_amove :     order -> bool
val is_asupportd : order -> bool
val is_asupporta : order -> bool
val is_fhold :     order -> bool
val is_fmove :     order -> bool
val is_fsupportd : order -> bool
val is_fsupporta : order -> bool
val is_fconvoy :   order -> bool

