signature ARCH =
sig

  datatype format = Int8 | Int16 | Int32 | Int64 | Float32 | Float64 | Address

  (*TODO: rename to stackframe*)
  type frame
  type access

  val newFrame : Temp.label * format list -> frame * access list

end

