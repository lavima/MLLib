(*
* filename: basic_transformations.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for basic numeric transformations 
*)

structure BasicTransformations =
struct

  type realRange = real * real
  
  (*
   * Transform a value within a given range to the range [0, 1]
   *)
  fun minmax ((min, max) : realRange) (x : real) : real
    = (x - min) / (max - min)

  (*
   * Transform a value within a given range to another range.
   *)
  fun rangeToRange (
    (sourceMin, sourceMax) : realRange,
    (targetMin, targetMax) : realRange)
    (x : real)
    = ((x - sourceMin) * (targetMax - targetMin)) / 
      (sourceMax - sourceMin) + targetMin
    

end
