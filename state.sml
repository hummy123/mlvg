signature STATE =
sig
  type t =
    { compositeOperation: CompositeOperation.t
    , shapeAntialias: bool
    , fill: Paint.t
    , stroke: Paint.t
    , strokeWidth: real
    , miterLimit: real
    , lineJoin: LineJoin.t
    , lineCap: LineCap.t
    , alpha: real
    , xform: real * real * real * real * real * real
    , scissor: Scissor.t
    }
end

structure State :> STATE =
struct
  type t =
    { compositeOperation: CompositeOperation.t
    , shapeAntialias: bool
    , fill: Paint.t
    , stroke: Paint.t
    , strokeWidth: real
    , miterLimit: real
    , lineJoin: LineJoin.t
    , lineCap: LineCap.t
    , alpha: real
    , xform: real * real * real * real * real * real
    , scissor: Scissor.t
    }
end
