(*
* file: print_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
*)

structure PrintUtil =
struct

  (*
  * Curried polymorphic print list function
  *)
  fun printList ( toString : 'a -> string )
                ( xs : 'a list ) 
      : unit =
  let
    fun iter( xs : 'a list ) : unit =
      case xs of
        [ x ] => print( toString x )
      | x::rxs => ( print( toString x ^ ", " ); iter rxs ) 
  in (
    print"[ "; 
    iter xs;
    print" ]" )
  end

  fun printArray ( toString : 'a -> string )
                 ( xs : 'a array ) 
      : unit =
  let
    fun iter( index : int ) : unit =
      if index<( ( Array.length xs )-1 ) then
        ( print( toString( Array.sub( xs, index ) ) ^ ", " ); iter( index+1 ) ) 
      else
        print( toString( Array.sub( xs, index ) ) )
  in (
    print"[ "; 
    iter 0;
    print" ]" )
  end

end (* structure PrintUtil *)
