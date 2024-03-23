structure Mlvg =
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

  fun triarea2 (ax, ay, bx, by, cx, cy) =
    let
      val abx = bx - ax
      val aby = by - ay
      val acx = cx - ax
      val acy = cy - ay
    in
      acx * aby - abx * acy
    end

  fun helpPolyArea (i, pts: Point.t vector, acc) =
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
end
