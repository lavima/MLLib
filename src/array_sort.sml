(*
* filename: array_sort.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with different array sorting algorithms.
*)

(* 
* TODO investigate whether this is needed, considering that an implementation 
* exists in the SML/NJ library
*)
structure ArraySort =
struct

  fun quick ( less : 'a * 'a -> bool ) ( xs : 'a array ) : 'a array =
  let
      
    fun partition( p : int, r : int ) : int =
    let
      val x = Array.sub( xs, r-1 )

      fun xch( i : int, j : int ) : unit =
      let
        val t = Array.sub( xs, i )
      in
        ( Array.update( xs, i, Array.sub( xs, j ) );
          Array.update( xs, j, t ) )
      end
      
      fun iter( i : int, j : int ) : int =
        case j<r-1 of
          false => i+1
        | true =>
            if less( x, Array.sub( xs, j ) ) then
              iter( i, j+1 )
            else (
              xch( i+1, j );
              iter( i+1, j+1 ) )
      
      val i = iter( p-1, p )
      val _ = xch( i, r-1 )
    in
      i
    end

    fun sort( p : int, r : int ) : 'a array = 
      case p<r-1 of 
        false => xs
      | true =>
        let
          val q = partition( p, r )
        in
          ( sort( p, q ); sort( q+1, r ) )
        end
  in
    sort( 0, Array.length xs )
  end

end (* structure ArraySort *)
