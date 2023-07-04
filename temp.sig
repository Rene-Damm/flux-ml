signature TEMP =
sig

  type temp
  type label

  val newTemp : unit -> temp
  val newLabel : unit -> label
  val newNamedLabel : string -> label

  val tempToString : temp -> string
  val labelToString : label -> string

end

