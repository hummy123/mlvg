type color = {r: real, g: real, b: real, a: real}

type paint =
  { xform: real * real * real * real * real * real
  , extent: real * real
  , radius: real
  , feather: real
  , innerColor: color
  , outerColor: color
  }

datatype command =
  MoveTo of {x: real, y: real}
| LineTo of {x: real, y: real}
| QuadTo of {cx: real, cy: real, x: real, y: real}
| BezierTo of {c1x: real, c1y: real, c2x: real, c2y: real, x: real, y: real}

datatype solidity = Solid | Hole

datatype winding = None | CCW | CW

datatype line_cap = Butt | Round | Square

datatype line_join = Miter | Round | Bevel

datatype blend_factor =
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

structure CompositeOperation =
struct
  datatype composite_operation =
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

  type t =
    { srcRgb: blend_factor
    , dstRgb: blend_factor
    , srcAlpha: blend_factor
    , dstAlpha: blend_factor
    }

  fun initFactors (sfactor, dfactor) =
    {srcRgb = sfactor, dstRgb = dfactor, srcAlpha = sfactor, dstAlpha = dfactor}

  fun initCompositeOperation operation =
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

type scissor =
  {xform: real * real * real * real * real * real, extent: real * real}

type state =
  { compositeOperation: CompositeOperation.t
  , fill: paint
  , stroke: paint
  , strokeWidth: real
  , strokeHeight: real
  , miterLimit: real
  , lineJoin: line_join
  , lineCap: line_cap
  , alpha: real
  , xform: real * real * real * real * real * real
  , scissor: scissor
  }

type point_flags = {corner: bool, left: bool, bevel: bool, innerBevel: bool}

type point =
  { x: real
  , y: real
  , dx: real
  , dy: real
  , len: real
  , dmx: real
  , dmy: real
  , flags: point_flags
  }

type context =
  { commands: real list
  , commandx: real
  , commandy: real
  , states: state list
  , tessTol: real
  , distTol: real
  , drawCallCount: int
  , fillTriCount: int
  , strokeTriCount: int
  , textTriCount: int
  }

type vertex = {x: real, y: real, u: real, v: real}

datatype texture_type = None | Alpha | Rgba

type path =
  { first: Word32.word
  , count: Word32.word
  , close: bool
  , nbevel: Word32.word
  , fill: vertex list
  , stroke: vertex list
  , winding: winding
  , convex: bool
  }

fun clamp (v, low, high) =
  if Real.< (v, low) then low else if Real.> (v, high) then high else v

fun solidityToWinding solidity =
  case solidity of
    Solid => CCW
  | Hole => CW

fun rgbToReal x =
  let val x = Real.fromInt x
  in Real./ (x, 255.0)
  end

fun rgba (r, g, b, a) =
  let
    val r = rgbToReal r
    val g = rgbToReal g
    val b = rgbToReal b
    val a = rgbToReal a
  in
    {r = r, g = g, b = b, a = a}
  end

fun rgb (r, g, b) =
  let
    val r = rgbToReal r
    val g = rgbToReal g
    val b = rgbToReal b
  in
    {r = r, g = g, b = b, a = 1.0}
  end

fun lerpRGBA (c0: color, c1: color, u) =
  let
    val a = clamp (u, 0.0, 1.0)
    val oma = 1.0 - a

    val r = a * (#r c0) + oma * (#r c1)
    val g = a * (#g c0) + oma * (#g c1)
    val b = a * (#b c0) + oma * (#b c1)
    val a = a * (#a c0) + oma * (#a c1)
  in
    {r = r, g = g, b = b, a = a}
  end

fun setAlpha (c0: color, a) =
  let
    val {r, g, b, ...} = c0
    val a = rgbToReal a
  in
    {r = r, g = g, b = b, a = a}
  end

fun setAlphaR (c0: color, a) =
  let val {r, g, b, ...} = c0
  in {r = r, g = g, b = b, a = a}
  end

fun getHue (h, m1, m2) =
  let
    val h = if h < 0.0 then h + 1.0 else if h > 1.0 then h - 1.0 else h
  in
    if Real.< (h, Real./ (1.0, 6.0)) then
      m1 + (m2 - m1) * h * 6.0
    else if Real.< (h, Real./ (3.0, 6.0)) then
      m2
    else if Real.< (h, Real./ (4.0, 6.0)) then
      m2 + (m2 - m1) * (Real./ (2.0, 3.0) - h) * 6.0
    else
      m1
  end

fun hsla (h, s, l, a) =
  let
    val h = if Real.< (h, 0.0) then h + 1.0 else h
    val s = clamp (s, 0.0, 1.0)
    val l = clamp (l, 0.0, 1.0)

    val m2 = if Real.<= (l, 0.5) then l * (s + 1.0) else l + s - l * s
    val m1 = 2.0 * l - m2

    val r = h + Real./ (1.0, 3.0)
    val r = clamp (getHue (r, m1, m2), 0.0, 1.0)
    val g = clamp (getHue (h, m1, m2), 0.0, 1.0)
    val b = h - Real./ (1.0, 3.0)
    val b = clamp (getHue (b, m1, m2), 0.0, 1.0)
    val a = rgbToReal a
  in
    {r = r, g = g, b = b, a = a}
  end

fun hsl (h, s, l) = hsla (h, s, l, 255)

val identity = (1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
