(*
* file: grayscale_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures that represents grayscale images.
*)

local

  (*
  * This structure specify grayscale images with a 8-bit word representing
  * each pixel.
  *)
  structure Word8GrayscaleImageSpec : IMAGE_SPEC =
  struct

    type pixel = Word8.word 

    val zeroPixel : pixel = 0w0

    val pixelAdd = Word8.+
    val pixelSub = Word8.-
    val pixelMul = Word8.*

    fun pixelScale( x : pixel, s : real ) : pixel =
      Word8.fromInt( Real.toInt IEEEReal.TO_NEAREST( real( Word8.toInt x )*s ) )

    fun pixelEqual( x : pixel, y : pixel ) : bool = 
      x=y

    fun pixelToString( x : pixel ) : string = 
      Word8.toString x

  end

  (*
  * This structure specify grayscale images with a 64-bit real representing
  * each pixel.
  *)
  structure RealGrayscaleImageSpec : IMAGE_SPEC =
  struct

    type pixel = real

    val zeroPixel = 0.0

    val pixelAdd = Real.+
    val pixelSub = Real.-
    val pixelMul = Real.*
    val pixelScale = Real.*

    fun pixelEqual( x : pixel, y : pixel ) : bool = 
      Util.eq elementCompare ( x, y )

    fun pixelToString( x : pixel ) : string =
      Real.fmt StringCvt.EXACT x

  end

  (*
  * This structure specify grayscale images with a integer representing
  * each pixel.
  *)
  structure IntGrayscaleImageSpec : IMAGE_SPEC =
  struct

    type pixel = int

    val zeroPixel = 0


    val pixelAdd = Int.+
    val pixelSub = Int.-
    val pixelMul = Int.*
    fun pixelScale( x : pixel, y : real) = Real.round(real x * y)

    fun pixelEqual( x : pixel, y : pixel ) : bool = 
      Util.eq elementCompare ( x, y )


    fun pixelToString( x : pixel ) : string = 
      Int.toString x

  end

in
  structure Word8GrayscaleImage = ImageFun( Word8GrayscaleImageSpec )
  structure RealGrayscaleImage = ImageFun( RealGrayscaleImageSpec )
  structure IntGrayscaleImage = ImageFun( IntGrayscaleImageSpec )
end

