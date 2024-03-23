signature PAINT =
sig
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
  type t =
    { xform: real * real * real * real * real * real
    , extent: real * real
    , radius: real
    , feather: real
    , innerColor: Color.t
    , outerColor: Color.t
    }
end
