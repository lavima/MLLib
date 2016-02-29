(* 
* filename: io.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains the signatures for image readers and writers.
*)

signature IMAGE_READER =
sig

  type 'a image

  val read : string -> ( 'a image ) option

end 

signature IMAGE_WRITER =
sig

  type 'a image

  val write : 'a image, string -> unit

end 
