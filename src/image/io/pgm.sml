(*
* file: pgm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures for loading images from PGM image files.
*)

signature PGM_IMAGE =
sig

  type image

  val fromList : int * int * word * word list -> image  
  val toList : image * word -> word list 

  val dimensions : image -> int * int

end (* signature PGM_IMAGE *)

functor PGMFun( Image : PGM_IMAGE ) : IMAGE_IO =
struct

  open Image

  type writeOptions = PNM.format * word

  fun read( filename : string ) : image option =
  let
    val input = BinIO.openIn filename

    val ( format, width, height, maxVal, _ ) = 
      PNMText.parseHeader input
  in
    case format of
      plainPGM => 
        SOME( fromList( width, height, maxVal, PNMText.parsePixels input ) )
    | rawPGM => 
        SOME( 
          fromList( 
            width, 
            height, 
            maxVal, 
            PNMBinary.readPixelsAsBytes( input, width*height ) ) )
    | _ => NONE
  end

  fun write' ( options as ( format, maxVal ) : writeOptions ) 
             ( im : image, filename : string ) : unit =
  let
    val format = 
      case options of
        plainPGM => format
      | rawPGM => format
      | _ => raise pnmException"Wrong format specified."

    val out = BinIO.openOut filename 
    val ( height, width ) = dimensions im
  in
    ( PNMText.writeHeader( out, ( format, width, height, 1, maxVal, [] ) );
      if format=plainPGM then
        PNMText.writeGrayscalePixels( out, toList im ) 
      else 
        PNMBinary.writeGrayscalePixels( out, toList im ) )
  end

  val write = write' ( plainPGM, 0w255 )

end (* functor PBMReaderFun *)

local 
  structure Word8Image : PGM_IMAGE =
  struct
    type image = GrayscaleImageWord8.image

    fun toList( im : image, maxVal : word ) : word list = 
    let
      val rfw = Real.fromInt o Word.toInt  
      val rfw8 = Real.fromInt o Word8.toInt  
      val wfr = Word.fromInt o Real.toInt IEEEReal.TO_NEAREST
    in
      GrayscaleImageWord8.foldr 
        ( fn( x, xs ) => 
            ( wfr( rfw8 x/255.0 )*rfw maxVal )::xs ) 
        []
        image
    end

    fun fromList( width : int, height : int, maxVal : word, pixels : word list )
        : image = 
    let
      val rfw = Real.fromInt o Word.toInt  
      val w8fr = Word8.fromInt o Real.toInt IEEEReal.TO_NEAREST
    in
      GrayscaleImageWord8.fromList'( 
        width, 
        height, 
        List.map ( fn x => w8fr( ( rfw x/rfw maxVal )*255.0 ) ) pixels )
    end
  end

  structure RealImage : PGM_IMAGE =
  struct
    type image = GrayscaleImageReal.image


    fun toList( im : image, maxVal : word ) : word list = 
    let
      val rfw = Real.fromInt o Word.toInt
      val wfr = Word.fromInt o Real.toInt IEEEReal.TO_NEAREST
    in
      GrayscaleImageReal.foldr 
        ( fn( x, xs ) => wfr( x*rfw maxVal )::xs ) 
        [] 
        im
    end

    fun fromList( width : int, height : int, maxVal : word, pixels : word list )
        : image = 
    let
      val rfw = Real.fromInt o Word.toInt
    in
      GrayscaleImageReal.fromList'( 
        width, 
        height, 
        List.map ( fn x => rfw x/rfw maxVal ) pixels )
    end
  end
in
  structure Word8PGM = PGMFun( Word8Image )
  structure RealPGM = PGMFun( RealImage )
end
