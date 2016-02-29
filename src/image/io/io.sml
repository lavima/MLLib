(* 
* filename: io.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains the signatures for image readers and writers.
*)

signature IMAGE_IO =
sig

  type image

  type writeOptions

  val read : string -> image option
  val write : image * string -> unit
  val write' : writeOptions -> image * string -> unit 

end 
