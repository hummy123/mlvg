signature WINDING =
sig
  datatype t = None | CCW | CW

  val fromSolidity: Solidity.t -> t
end

structure Winding :> WINDING =
struct
  open Solidity

  datatype t = None | CCW | CW

  fun fromSolidity (solidity: Solidity.t) =
    case solidity of
      Solid => CCW
    | Hole => CW
end
