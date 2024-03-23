signature COMMAND =
sig
  datatype t = BezierTo | Close | LineTo | MoveTo | Winding
  exception InvalidCommand

  val toValue: t -> real
  val fromValue: ((real * real -> bool) -> real -> bool) -> t
end

structure Command :> COMMAND =
struct
  datatype t = MoveTo | LineTo | BezierTo | Close | Winding
  exception InvalidCommand
  val (==) = Real.==

  fun toValue cmd =
    case cmd of
      MoveTo => 0.0
    | LineTo => 1.0
    | BezierTo => 2.0
    | Close => 3.0
    | Winding => 4.0

  fun fromValue num =
    if num == 0.0 then MoveTo
    else if num == 1.0 then LineTo
    else if num == 2.0 then BezierTo
    else if num == 3.0 then Close
    else if num == 4.0 then Winding
    else raise InvalidCommand
end
