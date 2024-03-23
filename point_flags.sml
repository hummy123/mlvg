signature POINT_FLAGS =
sig
  type t = {corner: bool, left: bool, bevel: bool, innerBevel: bool}

  val mk: {bevel: bool, corner: bool, innerBevel: bool, left: bool} -> t
end

structure PointFlags :> POINT_FLAGS =
struct
  type t = {corner: bool, left: bool, bevel: bool, innerBevel: bool}

  fun mk {corner, left, bevel, innerBevel} =
    {corner = corner, left = left, bevel = bevel, innerBevel = innerBevel}
end
