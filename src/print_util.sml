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
                ( Xs : 'a list ) 
      : unit =
  let
    fun iter( Xs : 'a list ) : unit =
      case Xs of
        [ X ] => print( toString X )
      | X::RXs => ( print( toString X ^ ", " ); iter RXs ) 
  in (
    print"[ "; 
    iter Xs;
    print" ]" )
  end

  fun printArray ( toString : 'a -> string )
                 ( A : 'a array ) 
      : unit =
  let
    fun iter( Index : int ) : unit =
      if Index<( ( Array.length A )-1 ) then
        ( print( toString( Array.sub( A, Index ) ) ^ ", " ); iter( Index+1 ) ) 
      else
        print( toString( Array.sub( A, Index ) ) )
  in (
    print"[ "; 
    iter 0;
    print" ]" )
  end

end (* structure PrintUtil *)
