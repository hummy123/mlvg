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

  val polyArea: t vector -> real
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

  fun helpPolyArea (i, pts: t vector, acc: real) =
    if i < Vector.length pts then
      let
        val p0 = Vector.sub (pts, 0)
        val p1 = Vector.sub (pts, i - 1)
        val p2 = Vector.sub (pts, i)
        val acc = MlvgMath.triarea2 (#x p0, #y p0, #x p1, #y p1, #x p2, #y p2)
      in
        helpPolyArea (i + 1, pts, acc)
      end
    else
      acc * 0.5

  fun polyArea pts = helpPolyArea (2, pts, 0.0)
end
