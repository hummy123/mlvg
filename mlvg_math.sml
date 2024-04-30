signature MLVG_MATH =
sig
  type real = Real32.real
  val clamp: real * real * real -> real
  val sign: real -> real
  val cross: int * int * int * int -> int
  val quantize: real * real -> real
  val ptEquals: int * int * int * int * int -> bool
  val distPtSeg: real * real * real * real * real * real -> real
  val getAverageScale: real * real * real * real * 'a * 'b -> real
  val triarea2: real * real * real * real * real * real -> real
  val polyReverse: 'a vector -> 'a vector
  val curveDivs: real * real * real -> Word32.word
  val degToRad: real -> real
  val radToDeg: real -> real
end

structure MlvgMath: MLVG_MATH =
struct
  fun polyReverse pts =
    let val length = Vector.length pts
    in Vector.tabulate (length, (fn idx => Vector.sub (pts, length - idx)))
    end

  fun cross (dx0, dy0, dx1, dy1) = dx1 * dy0 - dx0 * dy1

  fun ptEquals (x1, y1, x2, y2, tol) =
    let
      val dx = x2 - x1
      val dy = y2 - y1
      val lhs = dx * dx + dy
      val rhs = tol * tol
    in
      lhs < rhs
    end

  (* Most operations are done on reals, so it's convenient to open the module,
   * but the functions that don't use reals are above so they don't conflict
   * with the open. *)
  open Real32

  fun clamp (v, low, high) =
    if v < low then low else if v > high then high else v

  fun sign a : Real32.real =
    if a >= 0.0 then 1.0 else ~1.0

  fun quantize (a, d) =
    let
      val toRound = a / d
      val rounded = realRound toRound
    in
      rounded * d
    end

  fun distPtSeg (x, y, px, py, qx, qy) =
    let
      val pqx = qx - px
      val pqy = qy - py
      val dx = x - px
      val dy = y - py
      val d = pqx * pqx + pqy * pqy
      val t = pqx * dx + pqy * dy
      val t = if d > 0.0 then t / d else t
      val t = clamp (t, 0.0, 1.0)
      val dx = px + t * pqx - x
      val dy = py + t * pqy - y
    in
      dx * dx + dy * dy
    end

  fun getAverageScale (t0, t1, t2, t3, t4, t5) =
    let
      val sx = Math.sqrt (t0 * t0 + t2 * t2)
      val sy = Math.sqrt (t1 * t1 + t3 * t3)
    in
      (sx + sy) * 0.5
    end

  fun triarea2 (ax: real, ay: real, bx: real, by: real, cx: real, cy: real) =
    let
      val abx = bx - ax
      val aby = by - ay
      val acx = cx - ax
      val acy = cy - ay
    in
      acx * aby - abx * acy
    end

  fun curveDivs (r, arc, tol) =
    let
      val da = Math.acos (r / (r + tol)) * 2.0
      val ceil = ceil (arc / da)
      val result = Int.max (2, ceil)
    in
      Word32.fromInt result
    end

  fun degToRad deg = deg / 180.0 * Math.pi

  fun radToDeg rad = rad / Math.pi * 180.0
end
