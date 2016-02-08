(*
* filename: filter.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains the signature and matching structure for applying filters
* to images.
*)


signature FILTER_IMAGE =
sig
  
  type pixel
  type image = { Width : int, Height : int, Values : pixel Array.array }

  val ZeroPixel : pixel

  val createZero : int * int -> image
  
  val pixelAdd : pixel * pixel -> pixel
  val pixelMul : pixel * pixel -> pixel

end


signature FILTER =
sig

  exception filterException of string

  structure Image : FILTER_IMAGE

  val correlate : ImageCommon.borderExtension * ImageCommon.outputSize -> 
        Image.image * Image.image -> Image.image
  val convolve : ImageCommon.borderExtension * ImageCommon.outputSize -> 
        Image.image * Image.image -> Image.image

end



functor FilterFun( Image : FILTER_IMAGE ) : FILTER =
struct

  open ImageCommon

  exception filterException of string


  structure Image = Image

  local

    fun odd( X : int ) : bool = ( X mod 2 )=1

    fun filter( Image : Image.image, 
                Mask : Image.image, 
                BorderExtension : borderExtension,
                OutputShape : outputSize, 
                loopMask : int * int * int * Image.pixel -> Image.pixel ) 
        : Image.image =
    let

      val { Width, Height, Values } = Image
      val { Width=MaskWidth, Height=MaskHeight, Values=MaskPixels, ... } = Mask

      val Output as 
        { Width=OutputWidth, Height=OutputHeight, Values=OutputPixels, ... } = 
            Image.createZero( Width, Height )

      val TotalSize = Width*Height
      
      fun loop( Index : int ) =
        case Index<TotalSize of 
          false => ()
        | true => ( 
          let
            val X = Index mod Width
            val Y = Index div Width
            val Sum = loopMask( X, Y, 0, Image.ZeroPixel )
            val _ = Array.update( OutputPixels, Y*Width+X, Sum )
          in 
            loop( Index+1 )
          end )

      val _ = loop 0
    in
      Output
    end

    (*
    * Truncate an integer X to the interval [0,Max) 
    *)
    fun trunc( X : int, Max : int ) : int = 
      case X>=0 andalso X<Max of 
        true => X
      | false =>
          case X<0 of 
            true => 0
          | false => Max-1

  in (* local *)

    fun correlate ( BorderExtension : borderExtension, OutputSize : outputSize )
                  ( Image : Image.image, Mask : Image.image ) 
        : Image.image =
    let

      val { Width, Height, Values } = Image
      val { Width=MaskWidth, Height=MaskHeight, Values=MaskPixels } = Mask
      
      val CenterX = 
        if odd MaskWidth then 
          MaskWidth div 2 
        else 
          ( MaskWidth div 2 )-1
      val CenterY =  
        if odd MaskHeight then 
          MaskHeight div 2 
        else 
          ( MaskHeight div 2 )-1
      val MaskTotal = MaskWidth*MaskHeight

      fun loopMask( X : int, Y : int, Index : int, Sum : Image.pixel ) 
          : Image.pixel =
        case Index<MaskTotal of
          false => Sum
        | true => 
          let
            val XX = X+(Index mod MaskWidth-CenterX)
            val YY = Y+(Index div MaskWidth-CenterY)
            val ImageIndex = trunc( YY, Height )*Width+trunc( XX, Width )
          in
            loopMask( X, Y, Index+1, 
              Image.pixelAdd( 
                Sum, 
                Image.pixelMul( 
                  Array.sub( MaskPixels, Index ), 
                  Array.sub( Values, ImageIndex ) ) ) )
          end 
    in
      filter( Image, Mask, BorderExtension, OutputSize, loopMask )
    end


    (* 
    * Convolve an image with a two-dimensional mask 
    *)
    fun convolve ( BorderExtension : borderExtension, OutputSize : outputSize )
                 ( Image : Image.image, Mask : Image.image ) 
        : Image.image =
    let

      val { Width, Height, Values } = Image
      val { Width=MaskWidth, Height=MaskHeight, Values=MaskPixels } = Mask

      val CenterX = 
        if odd MaskWidth then 
          MaskWidth div 2 
        else 
          ( MaskWidth div 2 )-1
      val CenterY =  
        if odd MaskHeight then 
          MaskHeight div 2 
        else 
          ( MaskHeight div 2 )-1
      val MaskTotal = MaskWidth*MaskHeight

      fun loopMask( X : int, Y : int, Index : int, Sum : Image.pixel ) 
          : Image.pixel =
        case Index<MaskTotal of
          false => Sum
        | true => 
          let
            val RIndex = MaskTotal-1-Index
            val XX = X+( Index mod MaskWidth-CenterX )
            val YY = Y+( Index div MaskWidth-CenterY )
            val ImageIndex = trunc( YY, Height )*Width+trunc( XX, Width )
          in
            loopMask( X, Y, Index+1, 
              Image.pixelAdd( 
                Sum, 
                Image.pixelMul( 
                  Array.sub( MaskPixels, RIndex ), 
                  Array.sub( Values, ImageIndex ) ) ) )
          end 
    in
      filter( Image, Mask, BorderExtension, OutputSize, loopMask )
    end

  end (* local *)

end
