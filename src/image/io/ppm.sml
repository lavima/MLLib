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
  open PNM

  type writeOptions = format * word

  fun read( filename : string ) : image option =
  let
    val input = BinIO.openIn filename

    val ( format, width, height, maxVal, _ ) = 
      PNMText.parseHeader input
    val out = 
      case format of
        plainPPM => 
          SOME( 
            fromList( 
              height, 
              width, 
              maxVal, 
              PNMText.parseColorPixels input ) )
      | rawPPM => 
          SOME( 
            fromList( 
              height, 
              width, 
              maxVal, 
              PNMBinary.readColorPixels( input, maxVal, width*height ) ) )
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
        plainPPM => format
      | rawPPM => format
      | _ => raise pnmException"Wrong format specified."

    val out = BinIO.openOut filename 
    val ( height, width ) = dimensions im
    val _ = 
      ( PNMText.writeHeader( out, ( format, width, height, 3, maxVal, [] ) );
        if format=plainPPM then
          PNMText.writeColorPixels( out, toList( im, maxVal ) ) 
        else 
          PNMBinary.writeColorPixels( out, maxVal, toList( im, maxVal ) ) )
  in
    BinIO.closeOut out
  end

  val write = write' ( plainPPM, 0w255 )

end (* functor PPMFun *)

local
  structure Word8Image : PPM_IMAGE =
  struct
    type image = Word8RGBImage.image

    fun toList( im : image, maxVal : word ) : ( word * word * word ) list = 
    let
      val rfw = Real.fromInt o Word.toInt  
      val rfw8 = Real.fromInt o Word8.toInt  
      val wfr = Word.fromInt o Real.toInt IEEEReal.TO_NEAREST
      fun wfw( x : Word8.word ) : word =
        wfr( ( rfw8 x/255.0 )*rfw maxVal )  
    in
      List.rev(
        Word8RGBImage.fold Word8RGBImage.RowMajor 
          ( fn( ( r, g, b ), xs ) => 
              ( wfw r, wfw g, wfw b )::xs ) 
          []
          im )
    end

    fun fromList( height : int, 
                  width : int, 
                  maxVal : word, 
                  pixels : ( word * word * word ) list )
        : image = 
    let
      val rfw = Real.fromInt o Word.toInt  
      val w8fr = Word8.fromInt o Real.toInt IEEEReal.TO_NEAREST
      fun wfw( x : word ) : Word8.word =
        w8fr( ( rfw x/rfw maxVal )*255.0 )
    in
      Word8RGBImage.fromList'( 
        height, 
        width, 
        List.map ( fn( r, g, b ) => ( wfw r, wfw g, wfw b ) ) pixels )
    end

    val dimensions = Word8RGBImage.dimensions
  end

  structure RealImage : PPM_IMAGE =
  struct
    type image = RealRGBImage.image

    fun toList( im : image, maxVal : word ) : ( word * word * word ) list = 
    let
      val rfw = Real.fromInt o Word.toInt
      fun wfr( x : real ) : word = 
        Word.fromInt( Real.toInt IEEEReal.TO_NEAREST ( x*rfw maxVal ) )
    in
      List.rev(
        RealRGBImage.fold RealRGBImage.RowMajor
          ( fn( ( r, g, b ), xs ) => ( wfr r, wfr g, wfr b )::xs ) 
          [] 
          im )
    end

    fun fromList( height : int, 
                  width : int, 
                  maxVal : word, 
                  pixels : ( word * word * word ) list )
        : image = 
    let
      fun rfw( x : word ) : real = 
        real( Word.toInt x )/real( Word.toInt maxVal ) 
    in
      RealRGBImage.fromList'( 
        height, 
        width, 
        List.map ( fn( r, g, b ) => ( rfw r, rfw g, rfw b ) ) pixels )
    end

    val dimensions = RealRGBImage.dimensions
  end
in
  structure Word8PPM = PPMFun( Word8Image )
  structure RealPPM = PPMFun( RealImage )
end

