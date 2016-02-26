(*
* file: grayscale_pnm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures for loading grayscale images from PNM image 
* files.
*)

local 

  structure Common = 
  struct

    val depth = 1
    val format = PNMCommon.plainPGM

  end (* structure Common *)

  structure Word8Image : PNM_IMAGE =
  struct

    open Common

    type image = GrayscaleThresholdWord8.image  

    val createImage = fromList

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

  end

in
  structure GrayscalePNMWord8 = PNMFun( Word8Image )
end
