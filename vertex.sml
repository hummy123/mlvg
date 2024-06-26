signature VERTEX =
sig
  type real = Real32.real
  type t = {u: real, v: real, x: real, y: real}
  val make: {u: real, v: real, x: real, y: real} -> t
end

structure Vertex :> VERTEX =
struct
  open Real32
  type t = {x: real, y: real, u: real, v: real}

  fun make {x, y, u, v} =
    {x = x, y = y, u = u, v = v}
end
