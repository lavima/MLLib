(*
* filename: array_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure providing utilitary array functions
*)

structure ArrayUtil =
struct

  fun fill( arr : 'a array, x : 'a ) : unit =
    Array.modify ( fn _ => x ) arr

end (* structure ArrayUtil *)
