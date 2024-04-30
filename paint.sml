signature PAINT =
sig
  type real = Real32.real
  type t =
    { xform: real * real * real * real * real * real
    , extent: real * real
    , radius: real
    , feather: real
    , innerColor: Color.t
    , outerColor: Color.t
    }
end

structure Paint :> PAINT =
struct
  open Real32
  type t =
    { xform: real * real * real * real * real * real
    , extent: real * real
    , radius: real
    , feather: real
    , innerColor: Color.t
    , outerColor: Color.t
    }
end
