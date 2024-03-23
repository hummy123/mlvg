signature BLEND_FACTOR =
sig
  datatype t =
    Zero
  | One
  | SrcColor
  | OneMinusSrcColor
  | DstColor
  | OneMinusDstColor
  | SrcAlpha
  | OneMinusSrcAlpha
  | DstAlpha
  | OneMinusDstAlpha
  | SrcAlphaSaturate
end

structure BlendFactor :> BLEND_FACTOR =
struct
  datatype t =
    Zero
  | One
  | SrcColor
  | OneMinusSrcColor
  | DstColor
  | OneMinusDstColor
  | SrcAlpha
  | OneMinusSrcAlpha
  | DstAlpha
  | OneMinusDstAlpha
  | SrcAlphaSaturate
end
