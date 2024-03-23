signature COMPOSITE_OPERATION =
sig
  datatype t =
    SourceOver
  | SourceIn
  | SourceOut
  | Atop
  | DestinationOver
  | DestinationIn
  | DestinationOut
  | DestinationAtop
  | Lighter
  | Copy
  | Xor
end

structure CompositeOperation :> COMPOSITE_OPERATION =
struct
  datatype t =
    SourceOver
  | SourceIn
  | SourceOut
  | Atop
  | DestinationOver
  | DestinationIn
  | DestinationOut
  | DestinationAtop
  | Lighter
  | Copy
  | Xor
end
