(*
* filename: list_sort.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with different list sorting algorithms.
*)

structure ListSort =
struct

  fun insertion ( less : 'a * 'a -> bool ) ( xs : 'a list ) : 'a list =
  let
    fun insert( ys, x ) =
    case ys of 
      [] => [ x ]
    | y::ys' => 
        if less( y, x ) then
          y::insert( ys', x )
        else
          x::ys

    fun sort( xs : 'a list ) : 'a list = 
      case xs of 
        [ x ] => [ x ]
      | x::xs' => insert( sort xs', x )
  in
    sort xs
  end

end (* structure ListSort *)
