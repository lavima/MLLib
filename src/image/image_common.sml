(*
* file: image_common.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure that hold mappings that is shared throughout
* the image library.
*)

structure ImageCommon =
struct

  datatype borderExtension = zero | copy | wrap | mirror
  datatype outputSize = original | full

  datatype thresholdsMethod =
    percentage of int * real 
  | otsu of int 

  exception createException of string
  exception formatException of string
  exception mismatchException

end (* structure ImageCommon *)
