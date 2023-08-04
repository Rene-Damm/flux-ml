structure Temp : TEMP =
struct

  type temp = Symbol.symbol
  type label = Symbol.symbol

  val numTemps = ref 0
  val numLabels = ref 0

  fun newTemp () =
    let
      val i = !numTemps + 1
    in
      numTemps := !numTemps + 1;
      Symbol.create ("temp" ^ (Int.toString i))
    end

  fun newNamedLabel name =
    let 
      val i = !numLabels + 1
      fun replaceChar ch =
        case ch
          of #"+" => "_plus"
           | #"-" => "_minus"
           | #"/" => "_div"
           | #"*" => "_mul"
           | #"%" => "_mod"
           | #"=" => "_eq"
           | #"!" => "_not"
           | _ => Char.toString ch
      val name' = String.translate replaceChar name
    in
      numLabels := !numLabels + 1;
      Symbol.create (name' ^ (Int.toString i))
    end

  fun newLabel () = newNamedLabel "label"

  fun labelToString label = Symbol.toString label
  fun tempToString temp = Symbol.toString temp

end

