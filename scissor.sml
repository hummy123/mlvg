signature SCISSOR =
sig
  type real = Real32.real
  type t = {xform: real * real * real * real * real * real, extent: real * real}
end

structure Scissor :> SCISSOR =
struct
  open Real32
  type t = {xform: real * real * real * real * real * real, extent: real * real}
end
