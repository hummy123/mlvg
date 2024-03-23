signature LINE_CAP =
sig
  datatype t = Butt | Round | Square
end

structure LineCap :> LINE_CAP = struct datatype t = Butt | Round | Square end
