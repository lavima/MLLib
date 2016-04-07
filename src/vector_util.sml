(*
* file: vector_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains helpful vector related functions that cannot be found in 
* the SML BASIS library.
*)

structure VectorUtil =
struct

  fun toString ( tos : 'a -> string )
               ( xs : 'a vector ) 
      : string =
  let
    fun iter( i : int ) : string list =
      case i<( Vector.length xs-1 ) of
        false => [ tos( Vector.sub( xs, i ) ) ]
      | true => ( tos( Vector.sub( xs, i ) ) ^ ", " )::iter( i+1 )
  in
    "[ " ^ String.concat( iter 0 ) ^ " ]"
  end


end (* structure VectorUtil *)
