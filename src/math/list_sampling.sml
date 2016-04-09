(*
* file: list_sampling.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file contains an functionality for sampling from lists.
*)

structure ListSampling =
struct

  (*
   * Sample k items from a list using the reservoir algorithm.
   * This algorithm, originally developed by Donald Knuth has been prooven
   * to not be biased and works well on lists due to not needing the size
   * of the list.
   *)
  fun sampleK( randState : Random.rand, elements : 'a list, k : int ) 
    : 'a list =
  let
    val reservoir = Array.fromList( List.take( elements, k ) )
    
    fun buildReservoir( i : int, [] : 'a list ) : unit = ()
      | buildReservoir( i : int, ( e :: elems ) : 'a list ) : unit =
    let
      val j = Random.randRange ( 0, i ) randState
      val _ = if j < k then
                Array.update( reservoir, j, e )
              else ()
    in
      buildReservoir( i+1, elems )
    end
   
    val _ = buildReservoir( k, List.drop( elements, k )  )
  in
    List.tabulate( Array.length reservoir, fn x => Array.sub( reservoir, x ) )
  end

end
