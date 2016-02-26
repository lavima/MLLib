(*
* file: color_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures that represents color images.
*)


(*
* Structure for holding mappings shared between all color images
*)
structure ColorImageSpec = 
struct

  val depth = 3

  val pnmFormat = PNMCommon.plainPPM
  val pnmMaxVal = 0w255 

end (* structure GrayscaleImageSpecCommon *)

structure ColorImageWord8Spec : IMAGE_SPEC = 
struct
  
  open ColorImageSpec

  type image = { width : int, height : int, values : Word8.word Array.array }


  fun pixelAdd( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
    ( xr+yr, xg+yg, xb+yb )
  fun pixelSub( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
    ( xr-yr, xg-yg, xb-yb )
  fun pixelMul( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
    ( xr*yr, xg*yg, xb*yb )
  fun pixelMul'( x as (xr, xg, xb) : pixel, y : real) =
    raise ImageCommon.formatException "Not Implemented"

  val zeroPixel : pixel = ( 0w0, 0w0, 0w0 )


  fun pixelEqual( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) 
      : bool = 
  let
    val eq = Util.eq elementCompare
  in
    ( eq( xr, yr ) andalso eq( xg, yg ) andalso eq( xb, yb ) )
  end


  fun pixelFromWords( ws : word list, maxVal : word, invert : bool ) : pixel =
    case ws of 
      [ wr, wg, wb ] => 
        if not invert then
          ( Word8.fromInt( Word.toInt wr ),
            Word8.fromInt( Word.toInt wg ),
            Word8.fromInt( Word.toInt wb ) )
        else
          ( Word8.fromInt( Word.toInt( maxVal-wr ) ),
            Word8.fromInt( Word.toInt( maxVal-wg ) ),
            Word8.fromInt( Word.toInt( maxVal-wb ) ) )
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length ws ) )

  fun pixelToWords( x as ( xr, xg, xb ) : pixel, 
                    maxVal : word,
                    invert : bool) 
      : word list =
    if not invert then
      [ Word.fromInt( Word8.toInt xr ), 
        Word.fromInt( Word8.toInt xg ), 
        Word.fromInt( Word8.toInt xb ) ]
    else
      [ maxVal-Word.fromInt( Word8.toInt xr ), 
        maxVal-Word.fromInt( Word8.toInt xg ), 
        maxVal-Word.fromInt( Word8.toInt xb ) ]



  fun pixelToString( ( xr, xg, xb ) : pixel ) : string =
    "( " ^ Word8.toString xr ^ ", " ^ 
           Word8.toString xg ^ ", " ^
           Word8.toString xb ^ " )"

end (* struct ColorImageWord8Spec *)

structure ColorImageWord8 = ImageFun( ColorImageWord8Spec )


structure ColorImageRealSpec : IMAGE_SPEC =
struct

  open ColorImageSpec


  type image = { width : int, height : int, values : real Array.array }


  fun pixelAdd( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
    ( xr+yr, xg+yg, xb+yb )
  fun pixelSub( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
    ( xr-yr, xg-yg, xb-yb )
  fun pixelMul( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
    ( xr*yr, xg*yg, xb*yb )
  fun pixelMul'( x as ( xr, xg, xb ) : pixel, y : real ) = 
    ( xr*y, xg*y, xb*y )



  val zeroPixel = ( 0.0, 0.0, 0.0 )


  fun pixelEqual( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) 
      : bool = 
  let
    val eq = Util.eq elementCompare
  in
    ( eq( xr, yr ) andalso eq( xg, yg ) andalso eq( xb, yb ) )
  end


  fun pixelFromWords( ws : word list, maxVal : word, invert : bool ) : pixel =
  let
    val maxVal' = Real.fromLargeInt( Word.toLargeInt maxVal )
  in
    case ws of 
      [ wr, wg, wb ] => 
        if not invert then
          ( Real.fromInt( Word.toInt wr )/maxVal',
            Real.fromInt( Word.toInt wg )/maxVal',
            Real.fromInt( Word.toInt wb )/maxVal' )
        else 
          ( Real.fromInt( Word.toInt( maxVal-wr ) )/maxVal',
            Real.fromInt( Word.toInt( maxVal-wg ) )/maxVal',
            Real.fromInt( Word.toInt( maxVal-wb ) )/maxVal' )
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length ws ) )
  end

  fun pixelToWords( x as ( xr, xg, xb ) : pixel, 
                    maxVal : word,
                    invert : bool ) 
      : word list =
  let
    val _ = 
      if  xr>1.0 orelse xr<0.0 orelse 
          xg>1.0 orelse xg<0.0 orelse 
          xg>1.0 orelse xg<0.0 then
        raise ImageCommon.formatException"The pixel is not normalized"
      else 
        ()

    val maxVal' = Real.fromLargeInt( Word.toLargeInt maxVal )

    val wfli = Word.fromLargeInt
    val rtli = Real.toLargeInt IEEEReal.TO_NEAREST
  in
    if not invert then
      [ wfli( rtli(  xr*maxVal' ) ), 
        wfli( rtli( xg*maxVal' ) ), 
        wfli( rtli( xb*maxVal' ) ) ]
    else
      [ maxVal-wfli( rtli(  xr*maxVal' ) ), 
        maxVal-wfli( rtli( xg*maxVal' ) ), 
        maxVal-wfli( rtli( xb*maxVal' ) ) ]
  end


  fun pixelToString( ( xr, xg, xb ) : pixel ) : string =
    "( " ^ Real.toString xr ^ ", " ^ 
           Real.toString xg ^ ", " ^
           Real.toString xb ^ " )"


end (* struct ColorImageRealSpec *)

structure ColorImageReal = ImageFun( ColorImageRealSpec )
