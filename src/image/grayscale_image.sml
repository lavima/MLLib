(*
* file: grayscale_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures that represents grayscale images.
*)

structure GrayscaleImageSpec = 
struct

  val depth = 1

  val pnmFormat = PNMCommon.plainPGM
  val pnmMaxVal = 0w255 

end (* structure GrayscaleImageSpec *)

(*
* This structure specify grayscale images with a single 8-bit word representing
* each pixel.
*)
structure GrayscaleImageWord8Spec : IMAGE_SPEC =
struct

  open GrayscaleImageSpec

  type element = Word8.word 
  type pixel = element 
  type image = { width : int, height : int, values : pixel Array.array }

  val pixelAdd = Word8.+
  val pixelSub = Word8.-
  val pixelMul = Word8.*
  fun pixelMul' (x : pixel, y : real) = raise ImageCommon.formatException "Not implemented"
     (* Word8.fromInt(Real.round ((real Word8.toInt(x)) * y))*)

  val zeroPixel : pixel = 0w0

  fun pixelEqual( x : pixel, y : pixel ) : bool = 
    Util.eq elementCompare ( x, y )

  fun getElement( p : pixel, i : int ) : element =
    if i>0 orelse i<0 then
      raise ImageCommon.formatException"There is only one element"
    else
      p

  
  fun pixelFromWords( ws : word list, maxVal : word, invert : bool ) : pixel = 
    case ws of 
      [ w ] => 
        if not invert then
          Word8.fromInt( Word.toInt w ) 
        else
          Word8.fromInt( Word.toInt( maxVal-w ) ) 
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length ws ) )

  fun pixelToWords( x : pixel, maxVal : word, invert : bool ) 
      : word list =
  if not invert then
    [ Word.fromInt( Word8.toInt x ) ]
  else
    [ maxVal-Word.fromInt( Word8.toInt x ) ]


  fun pixelToString( x : pixel ) : string = 
    Word8.toString x

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
  type image = { width : int, height : int, values : pixel Array.array }


  val zeroPixel = 0


  val pixelAdd = Int.+
  val pixelSub = Int.-
  val pixelMul = Int.*
  fun pixelMul'( x : pixel, y : real) = Real.round(real x * y)

  fun pixelEqual( x : pixel, y : pixel ) : bool = 
    Util.eq elementCompare ( x, y )


  fun getElement( p : pixel, i : int ) : element =
    if i>0 orelse i<0 then
      raise ImageCommon.formatException"There is only one element"
    else
      p

  fun pixelFromWords( ws : word list, maxVal : word, invert : bool ) : pixel = 
    case ws of 
      [ w ] => 
        if not invert then
          Word.toInt w
        else
          Word.toInt( maxVal-w )
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length ws ) )

  fun pixelToWords( x : pixel, maxVal : word, invert : bool ) 
      : word list =
  let
    val maxVal' = Real.fromLargeInt( Word.toLargeInt maxVal )
    val maxInt = real( Option.valOf Int.maxInt )
    val minInt = real( Option.valOf Int.minInt )
    val wfr = Word.fromInt o Real.toInt IEEEReal.TO_NEAREST
  in
    if not invert then
      [ wfr( ( ( real x )-minInt )/( maxInt-minInt )*maxVal' ) ]
    else
      [ maxVal-wfr( ( ( real x )-minInt )/( maxInt-minInt )*maxVal' ) ]
  end

  fun pixelToString( x : pixel ) : string = 
    Int.toString x

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
  type image = { width : int, height : int, values : pixel Array.array }


  val zeroPixel = 0.0


  val pixelAdd = Real.+
  val pixelSub = Real.-
  val pixelMul = Real.*
  val pixelMul' = Real.*



  fun pixelEqual( x : pixel, y : pixel ) : bool = 
    Util.eq elementCompare ( x, y )

  fun getElement( p : pixel, i : int ) : element =
    if i>0 orelse i<0 then
      raise ImageCommon.formatException"There is only one element"
    else
      p


  fun pixelToWords( x : pixel, maxVal : word, invert : bool ) 
      : word list =
  let
    val _ = 
      if x>1.0 orelse x<0.0 then
        raise ImageCommon.formatException"The pixel is not normalized"
      else 
        ()

    val maxVal' = real( Word.toInt maxVal )
    val value = 
      if not invert then
        x*maxVal'
      else
        maxVal'-x*maxVal'
  in
    if Real.isFinite value then 
      [ Word.fromInt( Real.toInt IEEEReal.TO_NEAREST value ) ]
    else 
      [ 0w0 ]
  end

  fun pixelFromWords( ws : word list, maxVal : word, invert : bool ) : pixel = 
  let
    val rfw = Real.fromInt o Word.toInt
    val maxVal' = rfw maxVal 
  in
    case ws of 
      [ w ] => 
        if not invert then 
          ( rfw w )/maxVal'
        else
          rfw( maxVal-w )/maxVal' 
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length ws ) )
  end

  fun pixelToString( x : pixel ) : string =
    Real.fmt StringCvt.EXACT x

  fun test() = print "test";

end

structure GrayscaleImageReal = ImageFun( GrayscaleImageRealSpec )
