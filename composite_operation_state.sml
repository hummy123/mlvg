signature COMPOSITE_OPERATION_STATE =
sig
  type t =
    { srcRgb: BlendFactor.t
    , dstRgb: BlendFactor.t
    , srcAlpha: BlendFactor.t
    , dstAlpha: BlendFactor.t
    }

  val initFactors: BlendFactor.t * BlendFactor.t -> t

  val initOperation: CompositeOperation.t -> t
end

structure CompositeOperationState :> COMPOSITE_OPERATION_STATE =
struct
  open BlendFactor
  open CompositeOperation

  type t =
    { srcRgb: BlendFactor.t
    , dstRgb: BlendFactor.t
    , srcAlpha: BlendFactor.t
    , dstAlpha: BlendFactor.t
    }

  fun initFactors (sfactor, dfactor) =
    {srcRgb = sfactor, dstRgb = dfactor, srcAlpha = sfactor, dstAlpha = dfactor}

  fun initOperation operation =
    case operation of
      SourceOver => initFactors (One, OneMinusSrcAlpha)
    | SourceIn => initFactors (DstAlpha, Zero)
    | SourceOut => initFactors (OneMinusDstAlpha, Zero)
    | Atop => initFactors (DstAlpha, OneMinusSrcAlpha)
    | DestinationOver => initFactors (OneMinusDstAlpha, One)
    | DestinationIn => initFactors (Zero, SrcAlpha)
    | DestinationOut => initFactors (Zero, OneMinusSrcAlpha)
    | DestinationAtop => initFactors (OneMinusSrcAlpha, SrcAlpha)
    | Lighter => initFactors (One, One)
    | Copy => initFactors (One, Zero)
    | Xor => initFactors (OneMinusSrcAlpha, OneMinusSrcAlpha)
end
