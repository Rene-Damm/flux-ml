signature TEMP =
sig

  type temp
  type label

  val newTemp : unit -> temp
  val newLabel : unit -> label
  val newNamedLabel : string -> label

end

