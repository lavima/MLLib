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

 fun toString( toString : 'a -> string )
             ( xs : 'a array ) 
      : string =
  let
    fun iter( index : int ) : string =
      if index<( ( Array.length xs )-1 ) then
        toString( Array.sub( xs, index ) ) ^ ", " ^ iter( index+1 )
      else
         toString( Array.sub( xs, index ) )
  in
    "[ " ^ iter 0 ^ " ]"
  end

  

end (* structure ArrayUtil *)
