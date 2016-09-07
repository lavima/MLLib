(*
* filename: disjoint_set.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a SML implementation of the disjoint set algorithm 
* described in "Efficiency of a Good But Not Linear Set Union Algorithm" by
* Robert E. Tarjan
*)

structure DisjointSet =
struct
  
  type 'a element = int * int * real * 'a
  type 'a set = 'a element Array.array


  fun init( n : int, x : 'a ) : 'a set = 
    Array.tabulate( n, fn i => ( i, 0, 1.0, x ) )

  fun union( ds : 'a set, i1 : int, i2 : int ) : unit = 
  let
    val ( p1, r1, s1, x1 ) = Array.sub( ds, i1 )
    val ( p2, r2, s2, x2 ) = Array.sub( ds, i2 )
  in
    if p1=p2 then
      ()
    else if r1<r2 then (
      Array.update( ds, i1, ( i2, r1, s1, x1 ) );
      Array.update( ds, i2, ( p2, r2, s2+s1, x2 ) ) )
    else (
      Array.update( ds, i2, ( i1, r2, s2, x2 ) );
      if r1=r2 then
        Array.update( ds, i1, ( p1, r1+1, s1+s2, x1 ) )
      else
        Array.update( ds, i1, ( p1, r1, s1+s2, x1 ) ) )
  end

  fun find( ds: 'a set, i : int ) : 'a element = 
  let
    fun find'( i' : int ) : int =
    let
      val ( p, r, s, x ) = Array.sub( ds, i' )
    in
      case i'=p of 
        false => ( 
          case find' p of root => (
          Array.update( ds, i', ( root, r, s, x ) );
          root ) )
      | true => i'
    end
  in
    Array.sub( ds, find' i )
  end

  fun update( ds : 'a set, i : int, f : ( int * int * real * 'a ) -> 'a ) : unit =
  let
    val ( p, r, s, x ) = find( ds, i )
  in
    Array.update( ds, p, ( p, r, s, f( p, r, s, x ) ) )
  end

end (* structure DisjointSet *)
