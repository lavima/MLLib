(*
* file: cielab_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures that represents CIELab images.
*)

local
  structure RealSpec : IMAGE_SPEC =
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
      ( Real.==( xr, yr ) andalso Real.==( xg, yg ) andalso Real.==( xb, yb ) )

    fun pixelToString( ( xr, xg, xb ) : pixel ) : string =
      "( " ^ Real.toString xr ^ ", " ^ 
             Real.toString xg ^ ", " ^
             Real.toString xb ^ " )"
  end (* struct RealSpec *)
in
  structure RealCIELabImage = ImageFun( RealSpec )
end
