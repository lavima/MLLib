(*
* file: pbm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures for loading images from PBM image files.
*)

signature PBM_IMAGE =
sig

  type image

  val fromList : int * int * bool list -> image  
  val toList : image -> bool list 

  val dimensions : image -> int * int

end (* signature PBM_IMAGE *)

functor PBMFun( Image : PBM_IMAGE ) : IMAGE_IO =
struct

  open Image
  type writeOptions = PNM.format

  fun read( filename : string ) : image option =
  let
    val input = BinIO.openIn filename

    val ( format, width, height, _, _ ) = 
      PNMText.parseHeader input
  in
    case format of
      plainPBM => 
        fromList( width, height, PNMText.parseBooleanPixels input )
    | rawPBM => 
        fromList( 
          width, 
          height, 
          PNMBinary.readBooleanPixels( input, width*height ) )
  end

  fun write' ( options : writeOptions ) 
             ( im : image, filename : string ) : unit =
  let
    val format = 
      case options of
        plainPBM => options
      | rawPBM => options
      | _ => raise pnmException"Wrong format specified."

    val out = BinIO.openOut filename 
    val ( width, height ) = dimensions im
  in
    ( PNMText.writeHeader( out, ( format, width, height, 1, 0w1, [] ) );
      if format=plainPBM then
        PNMText.writeBooleanPixels( out, toList im ) 
      else 
        PNMBinary.writeBooleanPixels( out, toList im ) )
  end

  val write = write' plainPBM

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
