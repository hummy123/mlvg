signature SOLIDITY =
sig
  datatype t = Solid | Hole
end

structure Solidity :> SOLIDITY = struct datatype t = Solid | Hole end
