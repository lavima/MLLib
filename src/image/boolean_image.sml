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
  type image = { Width : int, Height : int, Values : pixel Array.array }


  val Depth = 1

  val ZeroPixel = false 

  val PNMFormat = PNMCommon.plainPBM
  val PNMMaxVal = 0w1 

  
  fun pixelAdd( X : bool, Y : bool ) : bool = ( X orelse Y )
  fun pixelSub( X : bool, Y : bool ) : bool = ( X andalso not Y ) 
  fun pixelMul( X : bool, Y : bool ) : bool = ( X andalso Y )
  fun pixelMul'( X : bool, Y : real) : bool = X
  
  fun elementCompare( X : element, Y : element ) : order =
    if X=Y then
      EQUAL
    else if X andalso not Y then
      GREATER
    else 
      LESS

  fun pixelEqual( X : pixel, Y : pixel ) : bool = 
    Util.eq elementCompare ( X, Y )

  fun getElement( Pixel : pixel, I : int ) : element =
    if I>0 orelse I<0 then
      raise ImageCommon.formatException"There is only one element"
    else
      Pixel

  fun elementFromReal X = 
    if X>0.0 then 
      true
    else
      false


  fun pixelFromWords( Ws : word list, MaxVal : word, Invert : bool ) : pixel =
    case Ws of 
      [ W ] => 
        if Invert then
          if W>0w0 then
            false
          else 
            true
        else
          if W>0w0 then
            true
          else 
            false
    | _ => raise ImageCommon.formatException( 
             "Unexpected number of words: " ^ Int.toString( List.length Ws ) )

  fun pixelToWords( X : pixel, MaxVal : word, Invert : bool ) 
      : word list =
    if Invert then
      if X then
        [ 0w0 ]
      else
        [ MaxVal ]
    else
      if X then
        [ MaxVal ]
      else
        [ 0w0 ]

  
  fun pixelToString( X : pixel ) : string = 
    Bool.toString X


end (* structure BooleanImageSpec *)

structure BooleanImage = ImageFun( BooleanImageSpec )

