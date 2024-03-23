signature PATH =
sig
  type t =
    { first: Word8.word
    , count: Word8.word
    , close: bool
    , nbevel: Word32.word
    , fill: Vertex.t vector
    , stroke: Vertex.t vector
    , winding: Winding.t
    , convex: bool
    }
end

structure Path :> PATH =
struct
  type t =
    { first: Word8.word
    , count: Word8.word
    , close: bool
    , nbevel: Word32.word
    , fill: Vertex.t vector
    , stroke: Vertex.t vector
    , winding: Winding.t
    , convex: bool
    }
end
