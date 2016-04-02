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
  open PNM

  type writeOptions = PNM.format

  fun read( filename : string ) : image option =
  let
    val input = BinIO.openIn filename

    val ( format, width, height, _, _ ) = 
      PNMText.parseHeader input

    val out = 
      case format of
        plainPBM => 
          SOME( fromList( height, width, PNMText.parseBooleanPixels input ) )
      | rawPBM => 
          SOME( 
            fromList( 
              height, 
              width, 
              PNMBinary.readBooleanPixels( input, width, height ) ) )
      | _ => NONE 

    val _ = BinIO.closeIn input
  in
    out
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
    val ( height, width ) = dimensions im
    val _ = 
      ( PNMText.writeHeader( out, ( format, width, height, 1, 0w1, [] ) );
        if format=plainPBM then
          PNMText.writeBooleanPixels( out, toList im ) 
        else 
          PNMBinary.writeBooleanPixels( out, width, toList im ) )
  in
    BinIO.closeOut out
  end

  val write = write' plainPBM

end (* functor PBMReaderFun *)

local 
  structure PBMImage : PBM_IMAGE =
  struct
    type image = BooleanImage.image
    val fromList = BooleanImage.fromList'
    val toList = 
      List.rev o
      BooleanImage.fold BooleanImage.RowMajor 
        ( fn( x, xs ) => x::xs ) 
        []
    val dimensions = BooleanImage.dimensions
  end
in
  structure BooleanPBM = PBMFun( PBMImage )
end
