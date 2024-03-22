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

type composite_operation_state =
  { srcRgb: blend_factor
  , dstRgb: blend_factor
  , srcAlpha: blend_factor
  , dstAlpha: blend_factor
  }

fun clamp (v, low, high) =
  if Real.< (v, low) then low else if Real.> (v, high) then high else v

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

type scissor =
  {xform: real * real * real * real * real * real, extent: real * real}

type state =
  { compositeOperation: composite_operation
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

