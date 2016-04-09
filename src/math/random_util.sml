(*
* file: random_util.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file utility functions for generating random structures.
*)

structure RandomUtil =
struct

  fun randomRealList ( length : int ) ( rand : Random.rand ) : real list =
    List.tabulate( length, fn i => Random.randReal rand )
  
  fun randomRealVector ( length : int ) ( rand : Random.rand ) : real vector =
    Vector.tabulate( length, fn i => Random.randReal rand )

  fun randomRealVectorRange 
    ( length : int, max : real vector, min : real vector)
    ( rand : Random.rand )
  : real vector =
    Vector.tabulate( length, 
      fn i => 
      let
        val iMax = Vector.sub( max, i )
        val iMin = Vector.sub( min, i )
        val trans = BasicTransformations.rangeToRange
                      ( ( 0.0, 1.0 ), ( iMin, iMax ) )
      in
        trans( Random.randReal rand )
      end )


end
