structure MlvgVector =
struct
  type real = Real32.real
  datatype t = Br of t vector | Lf of real vector

  val empty = Lf #[]

  fun toVectorHelp (Br br, acc) =
        toVectorHelpBranch (Vector.length br - 1, br, acc)
    | toVectorHelp (Lf lf, acc) = lf :: acc

  and toVectorHelpBranch (pos, br, acc) =
    if pos < 0 then
      acc
    else
      let val acc = toVectorHelp (Vector.sub (br, pos), acc)
      in toVectorHelpBranch (pos - 1, br, acc)
      end

  fun toVector vgVec =
    let val lst = toVectorHelp (vgVec, [])
    in Vector.concat lst
    end

  datatype append_result = Ok of t | Max of int

  (* 32 is the branching size used for Clojure's persistent vector,
   * which this is a simplified implementation of.
   * From benchmarks in changing the targetLength, 32 also seems the most
   * performant. *)
  val targetLength = 32

  fun createLinkHelp (ctr, depth, link) =
    if ctr < depth then createLinkHelp (ctr + 1, depth, Br #[link]) else link

  fun createLink (depth, insLf) =
    let val insLf = Lf insLf
    in createLinkHelp (0, depth - 1, insLf)
    end

  fun appendHelp (Br br, insLf: real vector) =
        let
          val length = Vector.length br
          val lastIdx = length - 1
          val last = Vector.sub (br, lastIdx)
          val result = appendHelp (last, insLf)
        in
          (case result of
             Ok newLast =>
               let
                 val newVec =
                   Vector.mapi
                     (fn (idx, el) => if idx = lastIdx then newLast else el) br
                 val newVec = Br newVec
               in
                 Ok newVec
               end
           | Max depth =>
               if length < targetLength then
                 let
                   val newEl = createLink (depth, insLf)
                   val newVec = Vector.tabulate (length + 1, (fn idx =>
                     if idx < length then Vector.sub (br, idx) else newEl))
                   val newNode = Br newVec
                 in
                   Ok newNode
                 end
               else
                 Max (depth + 1))
        end
    | appendHelp (Lf lf, insLf) =
        if Vector.length lf + Vector.length insLf < targetLength then
          let
            val newLf = Vector.concat [lf, insLf]
            val newLf = Lf newLf
          in
            Ok newLf
          end
        else if Vector.length lf = 0 then
          Ok (Lf insLf)
        else
          Max 1

  fun append (mlVec, insLf) =
    case appendHelp (mlVec, insLf) of
      Ok mlVec => mlVec
    | Max depth =>
        let val newEl = createLink (depth, insLf)
        in Br #[mlVec, newEl]
        end


  (* The below code is for testing balancing; 
   * it has been tested and balancing works as expected,
   * so it is commented out now.

  fun getLeafDepths (Br br, lst, num) =
        Vector.foldl (fn (el, lst) => getLeafDepths (el, lst, num + 1)) lst br
    | getLeafDepths (Lf lf, acc, num) =
        (num + 1) :: acc

  fun printLeafDepths mlVec =
    let
      val lst = getLeafDepths (mlVec, [], 0)
    in
      List.foldl
        (fn (el, _) =>
           let
             val str = Int.toString el
             val _ = print str
             val _ = print "\n"
           in
             ()
           end) () lst
    end

    *)


  (* The below code is for benchmarking to see what an ideal targetLength would
   * be (which targetLength would be the fastest). 
   * It's commented out now because benchmarking found that 32 would be fastest.

  fun insMany (ctr, limit, accVec) =
    if ctr = limit then
      accVec
    else
      let val accVec = append (accVec, #[Real32.fromInt ctr])
      in insMany (ctr + 1, limit, accVec)
      end

  fun bench () =
    let
      val startTime = Time.now ()
      val startTime = Time.toMicroseconds startTime

      val vec = insMany (0, 4000, empty)
      val vec = toVector vec

      val endTime = Time.now ()
      val endTime = Time.toMicroseconds endTime

      val timeDiff = endTime - startTime
      val timeDiff = LargeInt.toString timeDiff
      val timeTook = String.concat ["took ", timeDiff, " nanoseconds\n"]
      val _ = (print timeTook)
    in
      ()
    end

  val _ = bench()
  *)
end
