signature SCISSOR =
sig
  type t = {xform: real * real * real * real * real * real, extent: real * real}
end

structure Scissor :> SCISSOR =
struct
  type t = {xform: real * real * real * real * real * real, extent: real * real}
end
