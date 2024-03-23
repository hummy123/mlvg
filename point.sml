signature POINT =
sig
  type t =
    { x: real
    , y: real
    , dx: real
    , dy: real
    , len: real
    , dmx: real
    , dmy: real
    , flags: PointFlags.t
    }
end

structure Point :> POINT =
struct
  type t =
    { x: real
    , y: real
    , dx: real
    , dy: real
    , len: real
    , dmx: real
    , dmy: real
    , flags: PointFlags.t
    }
end
