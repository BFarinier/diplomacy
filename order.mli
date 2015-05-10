type utag = [ `Armies | `Fleets ]
type otag = [ `Convoy | `Hold | `Move | `SupportA | `SupportD ]
type order = private
    AHold of MapBoard.armies MapBoard.units
  | AMove of MapBoard.armies MapBoard.units *
      MapBoard.armies MapBoard.province
  | ASupportD of MapBoard.armies MapBoard.units *
      MapBoard.armies MapBoard.province
  | ASupportA of MapBoard.armies MapBoard.units *
      MapBoard.armies MapBoard.province * MapBoard.armies MapBoard.province
  | FHold of MapBoard.fleets MapBoard.units
  | FMove of MapBoard.fleets MapBoard.units *
      MapBoard.fleets MapBoard.province
  | FSupportD of MapBoard.fleets MapBoard.units *
      MapBoard.fleets MapBoard.province
  | FSupportA of MapBoard.fleets MapBoard.units *
      MapBoard.fleets MapBoard.province * MapBoard.fleets MapBoard.province
  | FConvoy of MapBoard.fleets MapBoard.units *
      MapBoard.armies MapBoard.province * MapBoard.armies MapBoard.province

val is_convoying : order -> bool

val tags : order -> [> utag ] * [> otag ]
val make : MapBoard.t -> [< utag ] -> [< otag ] -> string list -> order
