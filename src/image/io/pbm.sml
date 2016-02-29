(*
* file: pbm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures for loading boolean images from PBM image 
* files.
*)

signature PBM_IMAGE =
sig

  type image

  val fromList : int * int * bool list -> image  
  val toList : image -> bool list 

end (* signature PBM_IMAGE *)

functor PBMFun( Image : PBM_IMAGE ) : IMAGE_READER =
struct

  open Image

  fun read( filename : string ) : image option =
  let
    val input = BinIO.openIn filename

    val ( format, width, height, _, _ ) = 
      PNMText.parseHeader input
  in
    case format of
      plainPBM => fromList( PNMText.parseBooleanPixels input )
    | rawPBM => fromList( PNMBinary.readPixelsAsBits( input, width*height ) )
  end

  fun write( im : image, filename : string ) : unit =

end (* functor PBMReaderFun *)

local 
  structure PBMImage : PBM_IMAGE =
  struct
    type image = BooleanImage.image
    val fromList = BooleanImage.fromList'
    val toList = BooleanImage.foldr ( fn( x, xs ) => x::xs ) []
  end
in
  structure PBM = PBMFun( PBMImage )
end
