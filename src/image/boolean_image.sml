(*
* file: boolean_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures that represents boolean (black and white) 
* images.
*)

structure BooleanImageSpec : IMAGE_SPEC =
struct

  type element = bool
  type pixel = element
  type image = { width : int, height : int, values : pixel Array.array }


  val depth = 1

  val zeroPixel = false 

  val pnmFormat = PNMCommon.plainPBM
  val pnmMaxVal = 0w1 

  
  fun pixelAdd( x : bool, y : bool ) : bool = ( x orelse y )
  fun pixelSub( x : bool, y : bool ) : bool = ( x andalso not y ) 
  fun pixelMul( x : bool, y : bool ) : bool = ( x andalso y )
  fun pixelMul'( x : bool, y : real) : bool = x
  
  fun elementCompare( x : element, y : element ) : order =
    if x=y then
      EQUAL
    else if x andalso not y then
      GREATER
    else 
      LESS

  fun pixelEqual( x : pixel, y : pixel ) : bool = 
    Util.eq elementCompare ( x, y )

  fun getElement( pixel : pixel, i : int ) : element =
    if i>0 orelse i<0 then
      raise ImageCommon.formatException"There is only one element"
    else
      pixel

  fun elementFromReal x = 
    if x>0.0 then 
      true
    else
      false


  fun pixelFromWords( ws : word list, maxVal : word, invert : bool ) : pixel =
    case ws of 
      [ w ] => 
        if invert then
          if w>0w0 then
            false
          else 
            true
        else
          if w>0w0 then
            true
          else 
            false
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length ws ) )

  fun pixelToWords( x : pixel, maxVal : word, invert : bool ) 
      : word list =
    if invert then
      if x then
        [ 0w0 ]
      else
        [ maxVal ]
    else
      if x then
        [ maxVal ]
      else
        [ 0w0 ]

  
  fun pixelToString( x : pixel ) : string = 
    Bool.toString x


end (* structure BooleanImageSpec *)

structure BooleanImage = ImageFun( BooleanImageSpec )

