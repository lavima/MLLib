(* 
* file: image_real.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains structures providing functionality that is shared between 
* images with real elements.
*)

structure ImageReal =
struct

  val elementCompare = Real.compare

  fun elementFromReal X = X

end (* structure ImageReal *)
