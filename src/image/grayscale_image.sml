(*
* file: grayscale_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures that represents grayscale images.
*)

structure GrayscaleImageSpec = 
struct

  val Depth = 1

  val PNMFormat = PNMCommon.plainPGM
  val PNMMaxVal = 0w255 

end (* structure GrayscaleImageSpec *)

(*
* This structure specify grayscale images with a single 8-bit word representing
* each pixel.
*)
structure GrayscaleImageWord8Spec : IMAGE_SPEC =
struct

  open GrayscaleImageSpec
  open ImageWord8

  type element = Word8.word 
  type pixel = element 
  type image = { Width : int, Height : int, Values : pixel Array.array }

  val pixelAdd = Word8.+
  val pixelSub = Word8.-
  val pixelMul = Word8.*
  fun pixelMul' (x : pixel, y : real) = raise ImageCommon.formatException "Not implemented"
     (* Word8.fromInt(Real.round ((real Word8.toInt(x)) * y))*)

  val ZeroPixel : pixel = 0w0

  fun pixelEqual( X : pixel, Y : pixel ) : bool = 
    Util.eq elementCompare ( X, Y )

  fun getElement( Pixel : pixel, I : int ) : element =
    if I>0 orelse I<0 then
      raise ImageCommon.formatException"There is only one element"
    else
      Pixel

  
  fun pixelFromWords( Ws : word list, MaxVal : word, Invert : bool ) : pixel = 
    case Ws of 
      [ W ] => 
        if not Invert then
          Word8.fromInt( Word.toInt W ) 
        else
          Word8.fromInt( Word.toInt( MaxVal-W ) ) 
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length Ws ) )

  fun pixelToWords( X : pixel, MaxVal : word, Invert : bool ) 
      : word list =
  if not Invert then
    [ Word.fromInt( Word8.toInt X ) ]
  else
    [ MaxVal-Word.fromInt( Word8.toInt X ) ]


  fun pixelToString( X : pixel ) : string = 
    Word8.toString X

end

structure GrayscaleImageWord8 = ImageFun( GrayscaleImageWord8Spec )

(*
* This structure specify grayscale images with a integer representing
* each pixel.
*)
structure GrayscaleImageIntSpec : IMAGE_SPEC =
struct

  open ImageInt
  open GrayscaleImageSpec


  type element = int 
  type pixel = element 
  type image = { Width : int, Height : int, Values : pixel Array.array }


  val ZeroPixel = 0


  val pixelAdd = Int.+
  val pixelSub = Int.-
  val pixelMul = Int.*
  fun pixelMul'( x : pixel, y : real) = Real.round(real x * y)

  fun pixelEqual( X : pixel, Y : pixel ) : bool = 
    Util.eq elementCompare ( X, Y )


  fun getElement( Pixel : pixel, I : int ) : element =
    if I>0 orelse I<0 then
      raise ImageCommon.formatException"There is only one element"
    else
      Pixel

  fun pixelFromWords( Ws : word list, MaxVal : word, Invert : bool ) : pixel = 
    case Ws of 
      [ W ] => 
        if not Invert then
          Word.toInt W
        else
          Word.toInt( MaxVal-W )
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length Ws ) )

  fun pixelToWords( X : pixel, MaxVal : word, Invert : bool ) 
      : word list =
  let
    val MaxVal' = Real.fromLargeInt( Word.toLargeInt MaxVal )
    val MaxInt = real( Option.valOf Int.maxInt )
    val MinInt = real( Option.valOf Int.minInt )
    val wfr = Word.fromInt o Real.toInt IEEEReal.TO_NEAREST
  in
    if not Invert then
      [ wfr( ( ( real X )-MinInt )/( MaxInt-MinInt )*MaxVal' ) ]
    else
      [ MaxVal-wfr( ( ( real X )-MinInt )/( MaxInt-MinInt )*MaxVal' ) ]
  end

  fun pixelToString( X : pixel ) : string = 
    Int.toString X

end

structure GrayscaleImageInt = ImageFun( GrayscaleImageIntSpec )


(*
* This structure specify grayscale images with a 64-bit real representing
* each pixel.
*)
structure GrayscaleImageRealSpec : IMAGE_SPEC =
struct

  open GrayscaleImageSpec
  open ImageReal


  type element = real
  type pixel = element 
  type image = { Width : int, Height : int, Values : pixel Array.array }


  val ZeroPixel = 0.0


  val pixelAdd = Real.+
  val pixelSub = Real.-
  val pixelMul = Real.*
  val pixelMul' = Real.*



  fun pixelEqual( X : pixel, Y : pixel ) : bool = 
    Util.eq elementCompare ( X, Y )

  fun getElement( Pixel : pixel, I : int ) : element =
    if I>0 orelse I<0 then
      raise ImageCommon.formatException"There is only one element"
    else
      Pixel


  fun pixelToWords( X : pixel, MaxVal : word, Invert : bool ) 
      : word list =
  let
    val _ = 
      if X>1.0 orelse X<0.0 then
        raise ImageCommon.formatException"The pixel is not normalized"
      else 
        ()

    val MaxVal' = real( Word.toInt MaxVal )
    val Val = 
      if not Invert then
        X*MaxVal'
      else
        MaxVal'-X*MaxVal'
  in
    if Real.isFinite Val then 
      [ Word.fromInt( Real.toInt IEEEReal.TO_NEAREST Val ) ]
    else 
      [ 0w0 ]
  end

  fun pixelFromWords( Ws : word list, MaxVal : word, Invert : bool ) : pixel = 
  let
    val rfw = Real.fromInt o Word.toInt
    val MaxVal' = rfw MaxVal 
  in
    case Ws of 
      [ W ] => 
        if not Invert then 
          ( rfw W )/MaxVal'
        else
          rfw( MaxVal-W )/MaxVal' 
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length Ws ) )
  end

  fun pixelToString( X : pixel ) : string =
    Real.fmt StringCvt.EXACT X

  fun test() = print "test";

end

structure GrayscaleImageReal = ImageFun( GrayscaleImageRealSpec )
