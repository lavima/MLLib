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

  val Depth = 3

  val PNMFormat = PNMCommon.plainPPM
  val PNMMaxVal = 0w255 

end (* structure GrayscaleImageSpecCommon *)

structure ColorImageWord8Spec : IMAGE_SPEC = 
struct
  
  open ColorImageSpec
  open ImageWord8


  type element = Word8.word
  type pixel = element * element * element 
  type image = { Width : int, Height : int, Values : pixel Array.array }


  fun pixelAdd( X as ( XR, XG, XB ) : pixel, Y as ( YR, YG, YB ) : pixel ) = 
    ( XR+YR, XG+YG, XB+YB )
  fun pixelSub( X as ( XR, XG, XB ) : pixel, Y as ( YR, YG, YB ) : pixel ) = 
    ( XR-YR, XG-YG, XB-YB )
  fun pixelMul( X as ( XR, XG, XB ) : pixel, Y as ( YR, YG, YB ) : pixel ) = 
    ( XR*YR, XG*YG, XB*YB )
  fun pixelMul'( x as (xr, xg, xb) : pixel, y : real) =
    raise ImageCommon.formatException "Not Implemented"

  val ZeroPixel : pixel = ( 0w0, 0w0, 0w0 )


  fun pixelEqual( X as ( XR, XG, XB ) : pixel, Y as ( YR, YG, YB ) : pixel ) 
      : bool = 
  let
    val eq = Util.eq elementCompare
  in
    ( eq( XR, YR ) andalso eq( XG, YG ) andalso eq( XB, YB ) )
  end

  fun getElement( Pixel as ( R, G, B ) : pixel, I : int ) : element =
    if I>2 orelse I<0 then
      raise ImageCommon.formatException"There is only three elements"
    else if I=0 then
      R
    else if I=1 then
      G 
    else 
      B


  fun pixelFromWords( Ws : word list, MaxVal : word, Invert : bool ) : pixel =
    case Ws of 
      [ WR, WG, WB ] => 
        if not Invert then
          ( Word8.fromInt( Word.toInt WR ),
            Word8.fromInt( Word.toInt WG ),
            Word8.fromInt( Word.toInt WB ) )
        else
          ( Word8.fromInt( Word.toInt( MaxVal-WR ) ),
            Word8.fromInt( Word.toInt( MaxVal-WG ) ),
            Word8.fromInt( Word.toInt( MaxVal-WB ) ) )
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length Ws ) )

  fun pixelToWords( X as ( XR, XG, XB ) : pixel, 
                    MaxVal : word,
                    Invert : bool) 
      : word list =
    if not Invert then
      [ Word.fromInt( Word8.toInt XR ), 
        Word.fromInt( Word8.toInt XG ), 
        Word.fromInt( Word8.toInt XB ) ]
    else
      [ MaxVal-Word.fromInt( Word8.toInt XR ), 
        MaxVal-Word.fromInt( Word8.toInt XG ), 
        MaxVal-Word.fromInt( Word8.toInt XB ) ]



  fun pixelToString( ( XR, XG, XB ) : pixel ) : string =
    "( " ^ Word8.toString XR ^ ", " ^ 
           Word8.toString XG ^ ", " ^
           Word8.toString XB ^ " )"

end (* struct ColorImageWord8Spec *)

structure ColorImageWord8 = ImageFun( ColorImageWord8Spec )


structure ColorImageRealSpec : IMAGE_SPEC =
struct

  open ColorImageSpec
  open ImageReal


  type element = real
  type pixel = element * element * element 
  type image = { Width : int, Height : int, Values : pixel Array.array }


  fun pixelAdd( X as ( XR, XG, XB ) : pixel, Y as ( YR, YG, YB ) : pixel ) = 
    ( XR+YR, XG+YG, XB+YB )
  fun pixelSub( X as ( XR, XG, XB ) : pixel, Y as ( YR, YG, YB ) : pixel ) = 
    ( XR-YR, XG-YG, XB-YB )
  fun pixelMul( X as ( XR, XG, XB ) : pixel, Y as ( YR, YG, YB ) : pixel ) = 
    ( XR*YR, XG*YG, XB*YB )
  fun pixelMul'( x as ( XR, XG, XB ) : pixel, y : real ) = 
    ( XR*y, XG*y, XB*y )



  val ZeroPixel = ( 0.0, 0.0, 0.0 )


  fun pixelEqual( X as ( XR, XG, XB ) : pixel, Y as ( YR, YG, YB ) : pixel ) 
      : bool = 
  let
    val eq = Util.eq elementCompare
  in
    ( eq( XR, YR ) andalso eq( XG, YG ) andalso eq( XB, YB ) )
  end

  fun getElement( Pixel as ( R, G, B ) : pixel, I : int ) : element =
    if I>2 orelse I<0 then
      raise ImageCommon.formatException"There is only three elements"
    else if I=0 then
      R
    else if I=1 then
      G 
    else 
      B


  fun pixelFromWords( Ws : word list, MaxVal : word, Invert : bool ) : pixel =
  let
    val MaxVal' = Real.fromLargeInt( Word.toLargeInt MaxVal )
  in
    case Ws of 
      [ WR, WG, WB ] => 
        if not Invert then
          ( Real.fromInt( Word.toInt WR )/MaxVal',
            Real.fromInt( Word.toInt WG )/MaxVal',
            Real.fromInt( Word.toInt WB )/MaxVal' )
        else 
          ( Real.fromInt( Word.toInt( MaxVal-WR ) )/MaxVal',
            Real.fromInt( Word.toInt( MaxVal-WG ) )/MaxVal',
            Real.fromInt( Word.toInt( MaxVal-WB ) )/MaxVal' )
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length Ws ) )
  end

  fun pixelToWords( X as ( XR, XG, XB ) : pixel, 
                    MaxVal : word,
                    Invert : bool ) 
      : word list =
  let
    val _ = 
      if  XR>1.0 orelse XR<0.0 orelse 
          XG>1.0 orelse XG<0.0 orelse 
          XG>1.0 orelse XG<0.0 then
        raise ImageCommon.formatException"The pixel is not normalized"
      else 
        ()

    val MaxVal' = Real.fromLargeInt( Word.toLargeInt MaxVal )

    val wfli = Word.fromLargeInt
    val rtli = Real.toLargeInt IEEEReal.TO_NEAREST
  in
    if not Invert then
      [ wfli( rtli(  XR*MaxVal' ) ), 
        wfli( rtli( XG*MaxVal' ) ), 
        wfli( rtli( XB*MaxVal' ) ) ]
    else
      [ MaxVal-wfli( rtli(  XR*MaxVal' ) ), 
        MaxVal-wfli( rtli( XG*MaxVal' ) ), 
        MaxVal-wfli( rtli( XB*MaxVal' ) ) ]
  end


  fun pixelToString( ( XR, XG, XB ) : pixel ) : string =
    "( " ^ Real.toString XR ^ ", " ^ 
           Real.toString XG ^ ", " ^
           Real.toString XB ^ " )"


end (* struct ColorImageRealSpec *)

structure ColorImageReal = ImageFun( ColorImageRealSpec )
