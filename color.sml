signature COLOR =
sig
  type t = {r: real, g: real, b: real, a: real}
end

structure Color :> COLOR =
struct type t = {r: real, g: real, b: real, a: real} end
