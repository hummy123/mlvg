signature COMMAND =
sig
  datatype t =
    MoveTo of {x: real, y: real}
  | LineTo of {x: real, y: real}
  | BezierTo of {c1x: real, c1y: real, c2x: real, c2y: real, x: real, y: real}
  | Close
  | Winding

  val transform: t * (real * real * real * real * real * real) -> t
end

structure Command :> COMMAND =
struct
  datatype t =
    MoveTo of {x: real, y: real}
  | LineTo of {x: real, y: real}
  | BezierTo of {c1x: real, c1y: real, c2x: real, c2y: real, x: real, y: real}
  | Close
  | Winding

  fun transformX ((t0: real, t1: real, t2, t3, t4, t5), sx, sy) =
    sx * t0 + sy * t2 + t4

  fun transformY ((t0: real, t1: real, t2, t3, t4, t5), sx, sy) =
    sx * t1 + sy * t3 + t5

  fun transform (cmd, xform) =
    case cmd of
      MoveTo {x, y} =>
        let
          val x = transformX (xform, x, y)
          val y = transformY (xform, x, y)
        in
          MoveTo {x = x, y = y}
        end
    | LineTo {x, y} =>
        let
          val x = transformX (xform, x, y)
          val y = transformY (xform, x, y)
        in
          LineTo {x = x, y = y}
        end
    | BezierTo {c1x, c1y, c2x, c2y, x, y} =>
        let
          val c1x = transformX (xform, c1x, c1y)
          val c1y = transformY (xform, c1x, c1y)
          val c2x = transformX (xform, c2x, c2y)
          val c2y = transformY (xform, c2x, c2y)
          val x = transformX (xform, x, y)
          val y = transformY (xform, x, y)
        in
          BezierTo {c1x = c1x, c1y = c1y, c2x = c2x, c2y = c2y, x = x, y = y}
        end
    | _ => cmd
end
