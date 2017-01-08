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

  fun insertion ( less : 'a * 'a -> bool ) ( xs : 'a array ) : 'a array =
  let
    fun loop( i : int ) : 'a array = 
      case i<Array.length xs of
        false => xs
      | true =>
        let
          fun insert( j : int ) : unit =
            case j>0 andalso less( Array.sub( xs, j ), Array.sub( xs, j-1 ) ) of
              false => ()
            | true => 
              let
                val t = Array.sub( xs, j )
                val _ = Array.update( xs, j, Array.sub( xs, j-1 ) )
                val _ = Array.update( xs, j-1, t ) 
              in
                insert( j-1 )
              end
        in
          ( insert i; loop( i+1 ) )
        end
  in
    loop 1
  end

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
            if less( Array.sub( xs, j ), x ) then (
              xch( i+1, j );
              iter( i+1, j+1 ) )
            else 
              iter( i, j+1 )
      
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
