(*
* file: boolean_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures that represents boolean (black and white) 
* images.
*)

structure BooleanImageSpec : IMAGE_SPEC =
struct

  type pixel = bool 

  val zeroPixel = false 

  fun pixelAdd( x : bool, y : bool ) : bool = ( x orelse y )
  fun pixelSub( x : bool, y : bool ) : bool = 
    ( ( x andalso not y ) orelse ( not x andalso y )  ) 
  fun pixelMul( x : bool, y : bool ) : bool = ( x andalso y )
  fun pixelScale( x : bool, y : real) : bool = x
  fun pixelEqual( x : pixel, y : pixel ) : bool = x=y

  fun pixelToString( x : pixel ) : string = 
    Bool.toString x


end (* structure BooleanImageSpec *)

structure BooleanImage = ImageFun( BooleanImageSpec )

