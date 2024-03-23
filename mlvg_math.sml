signature MLVG_MATH =
sig
  val clamp: real * real * real -> real
  val sign: real -> real
  val cross: int * int * int * int -> int
  val quantize: real * real -> real
  val ptEquals: int * int * int * int * int -> bool
  val distPtSeg: real * real * real * real * real * real -> real
  val getAverageScale: real * real * real * real * 'a * 'b -> real
  val triarea2: real * real * real * real * real * real -> real
  val polyArea: Point.t vector -> real
  val polyReverse: 'a vector -> 'a vector
  val curveDivs: real * real * real -> Word32.word
end

structure MlvgMath: MLVG_MATH =
struct
  fun clamp (v, low, high) =
    if Real.< (v, low) then low else if Real.> (v, high) then high else v

  fun sign a =
    if a >= 0.0 then 1.0 else ~1.0

  fun cross (dx0, dy0, dx1, dy1) = dx1 * dy0 - dx0 * dy1

  fun quantize (a, d) =
    let
      val toRound = Real./ (a, d)
      val rounded = Real.realRound toRound
    in
      rounded * d
    end

  fun ptEquals (x1, y1, x2, y2, tol) =
    let
      val dx = x2 - x1
      val dy = y2 - y1
      val lhs = dx * dx + dy
      val rhs = tol * tol
    in
      lhs < rhs
    end

  fun distPtSeg (x, y, px, py, qx, qy) =
    let
      val pqx = qx - px
      val pqy = qy - py
      val dx = x - px
      val dy = y - py
      val d = pqx * pqx + pqy * pqy
      val t = pqx * dx + pqy * dy
      val t = if Real.> (d, 0.0) then t / d else t
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

  fun helpPolyArea (i, pts: Point.t vector, acc: real) =
    if i < Vector.length pts then
      let
        val p0 = Vector.sub (pts, 0)
        val p1 = Vector.sub (pts, i - 1)
        val p2 = Vector.sub (pts, i)
        val acc = triarea2 (#x p0, #y p0, #x p1, #y p1, #x p2, #y p2)
      in
        helpPolyArea (i + 1, pts, acc)
      end
    else
      acc * 0.5

  fun polyArea pts = helpPolyArea (2, pts, 0.0)

  fun polyReverse pts =
    let val length = Vector.length pts
    in Vector.tabulate (length, (fn idx => Vector.sub (pts, length - idx)))
    end

  fun curveDivs (r, arc, tol) =
    let
      val da = Math.acos (r / (r + tol)) * 2.0
      val ceil = Real.ceil (arc / da)
      val result = Int.max (2, ceil)
    in
      Word32.fromInt result
    end
end