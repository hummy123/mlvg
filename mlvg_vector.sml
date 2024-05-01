structure MlvgVector =
struct
  type real = Real32.real
  datatype t = Br of t vector | Lf of real vector

  fun toVectorHelp (Br br, acc) =
        toVectorHelpBranch (Vector.length br, br, acc)
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
        else
          Max 1
end
