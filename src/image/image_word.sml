(* 
* file: image_word.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains structures providing shared functionality to images with 
* word elements.
*)

structure ImageWord8 =
struct

  val elementCompare = Word8.compare

  fun elementFromReal X = Word8.fromInt( Real.trunc( X*255.0 ) )

end (* structure ImageWord8 *)
