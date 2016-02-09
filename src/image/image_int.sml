(* 
* file: image_word.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains structures providing shared functionality to images with 
* int elements.
*)

structure ImageInt =
struct

  val elementCompare = Int.compare

  fun elementFromReal x = Real.trunc( x*255.0 )

end (* structure ImageInt *)
