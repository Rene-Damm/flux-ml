structure Source =
struct

  type position = int

  (* Left (inclusive) and right (exclusive) position in source code. *)
  type region = position * position

  datatype file = File of { path: string }

  fun joinRegion (firstStart, firstEnd) (secondStart, secondEnd) =
    (Int.min(firstStart, secondStart), Int.max(firstEnd, secondEnd))

end

