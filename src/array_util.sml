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

  fun addReal'( arr1 : real array, arr2 : real array ) : unit =
    Array.modifyi( fn ( i, x ) => x + Array.sub( arr2, i ) ) arr1

  fun subtractReal'( arr1 : real array, arr2 : real array ) : unit =
    Array.modifyi( fn ( i, x ) => x - Array.sub( arr2, i ) ) arr1

end (* structure ArrayUtil *)
