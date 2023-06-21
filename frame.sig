(*TODO* rename FRAME to ARCH *)
signature FRAME =
sig

  datatype format = Int8 | Int16 | Int32 | Int64 | Float32 | Float64 | Address

  type frame
  type access

  val newFrame : Temp.label * format list -> frame

end

