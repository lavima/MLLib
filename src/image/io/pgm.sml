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
  open PNM

  type writeOptions = format * word

  fun read( filename : string ) : image option =
  let
    val input = BinIO.openIn filename

    val ( format, width, height, maxVal, _ ) = 
      PNMText.parseHeader input

    val out = 
      case format of
        plainPGM => 
          SOME( 
            fromList( 
              width, 
              height, 
              maxVal, 
              PNMText.parseGrayscalePixels input ) )
      | rawPGM => 
          SOME( 
            fromList( 
              width, 
              height, 
              maxVal, 
              PNMBinary.readGrayscalePixels( input, maxVal, width*height ) ) )
      | _ => NONE

    val _ = BinIO.closeIn input

  in
    out
  end

  fun write' ( options as ( format, maxVal ) : writeOptions ) 
             ( im : image, filename : string ) : unit =
  let
    val format = 
      case format of
        plainPGM => format
      | rawPGM => format
      | _ => raise pnmException"Wrong format specified."

    val out = BinIO.openOut filename 
    val ( height, width ) = dimensions im
    val _ = 
      ( PNMText.writeHeader( out, ( format, width, height, 1, maxVal, [] ) );
        if format=plainPGM then
          PNMText.writeGrayscalePixels( out, toList( im, maxVal ) ) 
        else 
          PNMBinary.writeGrayscalePixels( out, maxVal, toList( im, maxVal ) ) )
  in
    BinIO.closeOut out
  end

  val write = write' ( plainPGM, 0w255 )

end (* functor PBMReaderFun *)

local 
  structure Word8Image : PGM_IMAGE =
  struct
    type image = Word8GrayscaleImage.image

    fun toList( im : image, maxVal : word ) : word list = 
    let
      val rfw = Real.fromInt o Word.toInt  
      val rfw8 = Real.fromInt o Word8.toInt  
      val wfr = Word.fromInt o Real.toInt IEEEReal.TO_NEAREST
    in
      List.rev(
        Word8GrayscaleImage.fold Word8GrayscaleImage.RowMajor
          ( fn( x, xs ) => 
              wfr( ( rfw8 x/255.0 )*rfw maxVal )::xs ) 
          []
          im )
    end

    fun fromList( width : int, height : int, maxVal : word, pixels : word list )
        : image = 
    let
      val rfw = Real.fromInt o Word.toInt  
      val w8fr = Word8.fromInt o Real.toInt IEEEReal.TO_NEAREST
    in
      Word8GrayscaleImage.fromList'( 
        width, 
        height, 
        List.map ( fn x => w8fr( ( rfw x/rfw maxVal )*255.0 ) ) pixels )
    end

    val dimensions = Word8GrayscaleImage.dimensions
  end

  structure RealImage : PGM_IMAGE =
  struct
    type image = RealGrayscaleImage.image


    fun toList( im : image, maxVal : word ) : word list = 
    let
      val rfw = Real.fromInt o Word.toInt
      val wfr = Word.fromInt o Real.toInt IEEEReal.TO_NEAREST
    in
      List.rev( 
        RealGrayscaleImage.fold RealGrayscaleImage.RowMajor 
          ( fn( x, xs ) => wfr( x*rfw maxVal )::xs ) 
          [] 
          im )
    end

    fun fromList( width : int, height : int, maxVal : word, pixels : word list )
        : image = 
    let
      val rfw = Real.fromInt o Word.toInt
    in
      RealGrayscaleImage.fromList'( 
        width, 
        height, 
        List.map ( fn x => rfw x/rfw maxVal ) pixels )
    end

    val dimensions = RealGrayscaleImage.dimensions
  end
in
  structure Word8PGM = PGMFun( Word8Image )
  structure RealPGM = PGMFun( RealImage )
end
