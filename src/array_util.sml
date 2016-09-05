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
      else if index=( ( Array.length xs)-1 ) then
        toString( Array.sub( xs, index ) )
      else
        ""
  in
    "[ " ^ iter 0 ^ " ]"
  end

  fun allEq ( eq : 'a * 'a -> bool ) ( a1 : 'a array, a2 : 'a array ) : bool =
    ( Array.length a1=Array.length a2 ) andalso 
    ( Util.accumLoop
        ( fn( i, equal ) => 
            equal andalso eq( Array.sub( a1, i ), Array.sub( a2, i ) ) )
        true
        ( Array.length a1 ) )

end (* structure ArrayUtil *)
