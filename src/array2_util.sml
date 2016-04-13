(*
* file: array2_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains helpful Array2 functions that cannot be found in the SML 
* BASIS library.
*)

structure Array2Util =
struct
  
  type 'a array = 'a Array2.array

  fun toString ( tos : 'a -> string )
               ( xs : 'a array ) 
      : string =
  let
    fun iterR( i : int ) : string list = 
      case i<( Array2.nRows xs ) of 
        false => []
      | true => "[ "::iterC( i, 0, " ]"::iterR( i+1 ) ) 

    and iterC( i : int, j : int, ss : string list ) : string list =
      case j<( Array2.nCols xs-1 ) of
        false => tos( Array2.sub( xs, i, j ) )::ss
      | true => ( tos( Array2.sub( xs, i, j ) ) ^ ", " )::iterC( i, j+1, ss )
  in
    "[ " ^ String.concat( iterR 0 ) ^ " ]"
  end

end (* Array2Util *)
