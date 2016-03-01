(*
* file: ppm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures for loading images from PPM image files.
*)

signature PPM_IMAGE =
sig

  type image

  val fromList : int * int * word * ( word * word * word ) list -> image  
  val toList : image * word -> ( word * word * word ) list 

  val dimensions : image -> int * int

end (* signature PPM_IMAGE *)

functor PPMFun( Image : PPM_IMAGE ) : IMAGE_IO =
struct

  open Image

  fun read( filename : string ) : image option =
  let
    val input = BinIO.openIn filename

    val ( format, width, height, maxVal, _ ) = 
      PNMText.parseHeader input
  in
    case format of
      plainPPM => 
        SOME( fromList( width, height, maxVal, PNMText.parsePixels input ) )
    | rawPPM => 
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
        plainPPM => format
      | rawPPM => format
      | _ => raise pnmException"Wrong format specified."

    val out = BinIO.openOut filename 
    val ( height, width ) = dimensions im
  in
    ( PNMText.writeHeader( out, ( format, width, height, 3, maxVal, [] ) );
      if format=plainPGM then
        PNMText.writeColorPixels( out, toList im ) 
      else 
        PNMBinary.writeColorPixels( out, toList im ) )
  end

  val write = write' ( plainPPM, 0w255 )

end (* functor PPMFun *)

local
  structure Word8Image : PPM_IMAGE =
  struct
    type image = Word8RGBImage.image

    fun toList( im : image, maxVal : word ) : word list = 
    let
      val rfw = Real.fromInt o Word.toInt  
      val rfw8 = Real.fromInt o Word8.toInt  
      val wfr = Word.fromInt o Real.toInt IEEEReal.TO_NEAREST
      fun wfw( x : Word8.word ) : word =
        wfr( ( rfw8 x/255.0 )*rfw maxVal )  
    in
      GrayscaleImageWord8.foldr 
        ( fn( ( r, g, b ), xs ) => 
            ( wfw r, wfw g, wfw b )::xs ) 
        []
        image
    end

    fun fromList( width : int, height : int, maxVal : word, pixels : word list )
        : image = 
    let
      val rfw = Real.fromInt o Word.toInt  
      val w8fr = Word8.fromInt o Real.toInt IEEEReal.TO_NEAREST
      fun wfw( x : word ) : Word8.word =
        w8fr( ( rfw x/rfw maxVal )*255.0 )
    in
      GrayscaleImageWord8.fromList'( 
        width, 
        height, 
        List.map ( fn( r, g, b ) => ( wfw r, wfw g, wfw b ) ) pixels )
    end
  end

  structure RealImage : PPM_IMAGE =
  struct
    type image = RealRGBImage.image

    fun toList( im : image, maxVal : word ) : word list = 
    let
      val rfw = Real.fromInt o Word.toInt
      fun wfr( x : real ) : word = 
        Word.fromInt( Real.toInt IEEEReal.TO_NEAREST ( x*rfw maxVal ) )
    in
      GrayscaleImageReal.foldr 
        ( fn( ( r, g, b ), xs ) => ( wfr r, wfr g, wfr b )::xs ) 
        [] 
        im
    end

    fun fromList( width : int, height : int, maxVal : word, pixels : word list )
        : image = 
    let
      val rfw( x : word ) : real = 
        Real.fromInt( Word.toInt x )/Real.fromInt( Word.toInt maxVal ) 
    in
      GrayscaleImageReal.fromList'( 
        width, 
        height, 
        List.map ( fn( r, g, b ) => ( rfw r, rfw g, rfw b ) ) pixels )
    end
  end
in
  structure Word8PPM = PPMFun( Word8Image )
  structure RealPPM = PPMFun( RealImage )
end

