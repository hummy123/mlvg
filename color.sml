signature COLOR =
sig
  type real = Real32.real
  type t = {a: real, b: real, g: real, r: real}
  val rgba: int * int * int * int -> t
  val rgb: int * int * int -> t
  val lerpRGBA: t * t * real -> t
  val setAlpha: t * int -> t
  val setAlphaR: t * real -> t
  val getHue: real * real * real -> real
  val hsla: real * real * real * int -> t
  val hsl: real * real * real -> t
end

structure Color :> COLOR =
struct
  open Real32
  type t = {r: real, g: real, b: real, a: real}

  fun rgbToReal x =
    let val x = fromInt x
    in x / 255.0
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

  fun rgb (r: int, g: int, b: int) : t =
    let
      val r = rgbToReal r
      val g = rgbToReal g
      val b = rgbToReal b
    in
      {r = r, g = g, b = b, a = 1.0}
    end

  fun lerpRGBA (c0: t, c1: t, u) =
    let
      val a = MlvgMath.clamp (u, 0.0, 1.0)
      val oma = 1.0 - a

      val r = a * (#r c0) + oma * (#r c1)
      val g = a * (#g c0) + oma * (#g c1)
      val b = a * (#b c0) + oma * (#b c1)
      val a = a * (#a c0) + oma * (#a c1)
    in
      {r = r, g = g, b = b, a = a}
    end

  fun setAlpha (c0: t, a) : t =
    let
      val {r, g, b, ...} = c0
      val a = rgbToReal a
    in
      {r = r, g = g, b = b, a = a}
    end

  fun setAlphaR (c0: t, a) : t =
    let val {r, g, b, ...} = c0
    in {r = r, g = g, b = b, a = a}
    end

  fun getHue (h, m1, m2) =
    let
      val h = if h < 0.0 then h + 1.0 else if h > 1.0 then h - 1.0 else h
    in
      if h < 1.0 / 6.0 then m1 + (m2 - m1) * h * 6.0
      else if h < 3.0 / 6.0 then m2
      else if h < 4.0 / 6.0 then m2 + (m2 - m1) * ((2.0 / 3.0) - h) * 6.0
      else m1
    end

  fun hsla (h, s, l, a) =
    let
      val h = if h < 0.0 then h + 1.0 else h
      val s = MlvgMath.clamp (s, 0.0, 1.0)
      val l = MlvgMath.clamp (l, 0.0, 1.0)

      val m2 = if l <= 0.5 then l * (s + 1.0) else l + s - l * s
      val m1 = 2.0 * l - m2

      val r = h + 1.0 / 3.0
      val r = MlvgMath.clamp (getHue (r, m1, m2), 0.0, 1.0)
      val g = MlvgMath.clamp (getHue (h, m1, m2), 0.0, 1.0)
      val b = h - 1.0 / 3.0
      val b = MlvgMath.clamp (getHue (b, m1, m2), 0.0, 1.0)
      val a = rgbToReal a
    in
      {r = r, g = g, b = b, a = a}
    end

  fun hsl (h, s, l) = hsla (h, s, l, 255)
end
