(*
* filename: filter.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains the signature and matching structure for applying filters
* to images.
*)


signature FILTER_IMAGE =
sig
  
  type 'a image

  val zeroPixel : pixel

  val createZero : int * int -> 'a image
  
  val pixelAdd : pixel * pixel -> pixel
  val pixelMul : pixel * pixel -> pixel

end


signature FILTER =
sig

  type 'a image

  exception filterException of string

  val correlate : ImageCommon.borderExtension * ImageCommon.outputSize -> 
        'a image * 'a image -> 'a image
  val convolve : ImageCommon.borderExtension * ImageCommon.outputSize -> 
        'a image * 'a image -> 'a image

end



functor FilterFun( Image : FILTER_IMAGE ) : FILTER =
struct

  open ImageCommon

  exception filterException of string

  local

    fun odd( x : int ) : bool = ( x mod 2 )=1

    fun filter( im : 'a image, 
                mask : 'a image, 
                extension : borderExtension,
                outputShape : outputSize, 
                loopMask : int * int * int * 'a -> 'a ) 
        : 'a image =
    let

      val { width, height, values } = im
      val { width=maskWidth, height=maskHeight, values=maskPixels, ... } = mask

      val output as 
        { width=outputWidth, height=outputHeight, values=outputPixels, ... } = 
            Image.createZero( width, height )

      val totalSize = width*height
      
      fun loop( index : int ) =
        case index<totalSize of 
          false => ()
        | true => ( 
          let
            val x = index mod width
            val y = index div width
            val sum = loopMask( x, y, 0, Image.zeroPixel )
            val _ = Array.update( outputPixels, y*width+x, sum )
          in 
            loop( index+1 )
          end )

      val _ = loop 0
    in
      output
    end

    (*
    * Truncate an integer x to the interval [0,max) 
    *)
    fun trunc( x : int, max : int ) : int = 
      case x>=0 andalso x<max of 
        true => x
      | false =>
          case x<0 of 
            true => 0
          | false => max-1

  in (* local *)

    fun correlate ( extension : borderExtension, outputSize : outputSize )
                  ( im : 'a image, mask : 'a image ) 
        : 'a image =
    let

      val { width, height, values } = im
      val { width=maskWidth, height=maskHeight, values=maskPixels } = mask
      
      val centerX = 
        if odd maskWidth then 
          maskWidth div 2 
        else 
          ( maskWidth div 2 )-1
      val centerY =  
        if odd maskHeight then 
          maskHeight div 2 
        else 
          ( maskHeight div 2 )-1
      val maskTotal = maskWidth*maskHeight

      fun loopMask( x : int, y : int, index : int, sum : Image.pixel ) 
          : Image.pixel =
        case index<maskTotal of
          false => sum
        | true => 
          let
            val xx = x+(index mod maskWidth-centerX)
            val yy = y+(index div maskWidth-centerY)
            val imageIndex = trunc( yy, height )*width+trunc( xx, width )
          in
            loopMask( x, y, index+1, 
              Image.pixelAdd( 
                sum, 
                Image.pixelMul( 
                  Array.sub( maskPixels, index ), 
                  Array.sub( values, imageIndex ) ) ) )
          end 
    in
      filter( im, mask, extension, outputSize, loopMask )
    end


    (* 
    * Convolve an image with a two-dimensional mask 
    *)
    fun convolve ( extension : borderExtension, outputSize : outputSize )
                 ( im : 'a image, mask : 'a image ) 
        : 'a image =
    let

      val { width, height, values } = im
      val { width=maskWidth, height=maskHeight, values=maskPixels } = mask

      val centerX = 
        if odd maskWidth then 
          maskWidth div 2 
        else 
          ( maskWidth div 2 )-1
      val centerY =  
        if odd maskHeight then 
          maskHeight div 2 
        else 
          ( maskHeight div 2 )-1
      val maskTotal = maskWidth*maskHeight

      fun loopMask( x : int, y : int, index : int, sum : 'a ) 
          : 'a =
        case index<maskTotal of
          false => sum
        | true => 
          let
            val rindex = maskTotal-1-index
            val xx = x+( index mod maskWidth-centerX )
            val yy = y+( index div maskWidth-centerY )
            val imageIndex = trunc( yy, height )*width+trunc( xx, width )
          in
            loopMask( x, y, index+1, 
              Image.pixelAdd( 
                sum, 
                Image.pixelMul( 
                  Array.sub( maskPixels, rindex ), 
                  Array.sub( values, imageIndex ) ) ) )
          end 
    in
      filter( im, mask, extension, outputSize, loopMask )
    end

  end (* local *)

end
