signature LINE_JOIN =
sig
  datatype t = Miter | Round | Bevel
end

structure LineJoin :> LINE_JOIN = struct datatype t = Miter | Round | Bevel end
