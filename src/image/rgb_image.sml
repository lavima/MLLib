(*
* file: rgb_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures that represents RGB images.
*)

local
  structure Word8RGBImageSpec : IMAGE_SPEC = 
  struct
    
    open Word8

    type pixel = word * word * word

    val zeroPixel : pixel = ( 0w0, 0w0, 0w0 )

    fun pixelAdd( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
      ( xr+yr, xg+yg, xb+yb )

    fun pixelSub( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
      ( xr-yr, xg-yg, xb-yb )

    fun pixelMul( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
      ( xr*yr, xg*yg, xb*yb )

    fun pixelScale( x as ( r, g, b ) : pixel, y : real ) : pixel =
    let
      fun scale( x : word, y : word ) : word =  
        fromInt( Real.toInt IEEEReal.TO_NEAREST( real( toInt x )*s ) )
    in
      ( scale r, scale g, scale b ) 
    end

    fun pixelEqual( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) 
        : bool = 
    let
      val eq = Util.eq elementCompare
    in
      ( eq( xr, yr ) andalso eq( xg, yg ) andalso eq( xb, yb ) )
    end

    fun pixelToString( ( xr, xg, xb ) : pixel ) : string =
      "( " ^ Word8.toString xr ^ ", " ^ 
             Word8.toString xg ^ ", " ^
             Word8.toString xb ^ " )"

  end (* struct Word8RGBImageSpec *)

  structure RealRGBImageSpec : IMAGE_SPEC =
  struct

    type pixel = real * real * real

    fun pixelAdd( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
      ( xr+yr, xg+yg, xb+yb )
    fun pixelSub( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
      ( xr-yr, xg-yg, xb-yb )
    fun pixelMul( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) = 
      ( xr*yr, xg*yg, xb*yb )

    fun pixelScale( x as ( xr, xg, xb ) : pixel, y : real ) = 
      ( xr*y, xg*y, xb*y )

    val zeroPixel = ( 0.0, 0.0, 0.0 )

    fun pixelEqual( x as ( xr, xg, xb ) : pixel, y as ( yr, yg, yb ) : pixel ) 
        : bool = 
    let
      val eq = Util.eq elementCompare
    in
      ( eq( xr, yr ) andalso eq( xg, yg ) andalso eq( xb, yb ) )
    end

    fun pixelToString( ( xr, xg, xb ) : pixel ) : string =
      "( " ^ Real.toString xr ^ ", " ^ 
             Real.toString xg ^ ", " ^
             Real.toString xb ^ " )"

  end (* struct RealRGBImageSpec *)
in
  structure Word8RGBImage = ImageFun( Word8RGBImageSpec )
  structure RealRGBImage = ImageFun( RealRGBImageSpec )
end
