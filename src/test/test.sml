(*
* filename: test.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no> 
*
* This file contains the signature for all test types.
*)

signature TEST =
sig
  type ( 'a, 'b )testSpec

  val test : ( 'a, 'b )testSpec -> unit
  val test' : string list -> ( 'a, 'b )testSpec -> unit
end
