(*
* file: image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains the image signature and the image type functor used
* to create new image types.
*)


(* 
* This signature specify all the mappings shared by all image types.
*)
signature IMAGE =
sig

  datatype traversal = 
    RowMajor | 
    ColMajor

  datatype borderExtension = 
    ZeroExtension | 
    CopyExtension | 
    WrapExtension

  datatype outputSize = 
    OriginalSize | 
    FullSize

  type pixel
  type image 

  exception mismatchException

  val image : int * int * pixel -> image
  val zeroImage : int * int -> image

  val dimensions : image -> int * int

  val fromList : int * int * pixel list list -> image
  val fromList' : int * int * pixel list -> image

  val transposed : image -> image

  val sub : image * int * int -> pixel
  val update : image * int * int * pixel -> unit

  val add : image * image -> image 
  val add' : image * image -> unit 
  val subtract : image * image -> image
  val subtract' : image * image -> unit

  val app : ( pixel -> unit ) -> image -> unit
  val appi : ( int * pixel -> unit ) -> image -> unit
  val fold : ( pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldi : ( int * pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val modify : ( pixel -> pixel ) -> image -> unit
  val modifyi : ( int * pixel -> pixel ) -> image -> unit
  val tabulate : (int * int * (int * int -> pixel))  -> image

  val fill : image * pixel -> unit

  val correlate : borderExtension * outputSize -> image * image -> image
  val convolve : borderExtension * outputSize -> image * image -> image

  val equal : image * image -> bool

  val toString : image -> string

  val rotate : (image * real) -> image
  val rotateCrop : (image * real * int * int) -> image

end


(* 
* This signature specify the interface for creating new image types.
*)
signature IMAGE_SPEC = 
sig
  
  type pixel 
  
  val zeroPixel : pixel

  val pixelAdd : pixel * pixel -> pixel
  val pixelSub : pixel * pixel -> pixel
  val pixelMul : pixel * pixel -> pixel
  val pixelScale : pixel * real -> pixel
  val pixelEqual : pixel * pixel -> bool

  val pixelToString : pixel -> string

end

(*
* This functor is used to create all the image types. The functor takes a 
* structure matching the IMAGE_SPEC signature as parameter.
*)
functor ImageFun( Spec : IMAGE_SPEC ) : IMAGE = 
struct

  open Array2
  open Spec

  datatype borderExtension = 
    ZeroExtension | 
    CopyExtension | 
    WrapExtension

  datatype outputSize = 
    OriginalSize | 
    FullSize

  type image = pixel array
  
  exception mismatchException

  fun full( im : image ) : pixel region =
    { base=im, row=0, col=0, nrows=NONE, ncols=NONE }

  fun image( width : int, height : int, x : pixel ) : image = 
    Array2.array( width, height, x )

  fun fromList'( width : int, height : int, pixels : pixel list ) : image =
  let
    fun build( xs : pixel list ) : pixel list list =
      List.take( xs, width )::build( List.drop( xs, width ) )
  in
    fromList( build pixels )
  end

  fun zeroImage( width : int, height : int ) : image = 
    image( width, height, zeroPixel )

  fun add( im1 : image, im2 : image ) : image = 
  let
    val ( height1, width1 ) = dimensions im1
    val ( height2, width2 ) = dimensions im2
  in
    if width1=width2 andalso height1=height2 then
    let
      val output = zeroImage( width1, height1 )
      val _ = 
        modifyi RowMajor
          ( fn( i, j, _ ) => pixelAdd( sub( im1, i, j ), sub( im2, i, j ) ) )
          ( full output )
    in
      output
    end
    else
      raise mismatchException
  end

  fun add'( im1 : image, im2 : image ) : image = 
  let
    val ( height1, width1 ) = dimensions im1
    val ( height2, width2 ) = dimensions im2
  in
    if width1=width2 andalso height1=height2 then
      ( modifyi RowMajor 
          ( fn( i, j, p1 ) => pixelAdd( p1, sub( im2, i, j ) ) )
          ( full im1 ) ;
        im1 )
    else
      raise mismatchException
  end

  fun subtract( im1 : image, im2 : image ) : image = 
  let
    val ( height1, width1 ) = dimensions im1
    val ( height2, width2 ) = dimensions im2
  in
    if width1=width2 andalso height1=height2 then
    let
      val output = zeroImage( width1, height1 )
      val _ = 
        modifyi RowMajor
          ( fn( i, j, _ ) => pixelSub( sub( im1, i, j ), sub( im2, i, j ) ) )
          ( full output )
    in
      output
    end
    else
      raise mismatchException
  end

  fun subtract'( im1 : image, im2 : image ) : image = 
  let
    val ( height1, width1 ) = dimensions im1
    val ( height2, width2 ) = dimensions im2
  in
    if width1=width2 andalso height1=height2 then
      ( modifyi RowMajor
          ( fn( i, j, p1 ) => pixelSub( p1, sub( im2, i, j ) ) )
          ( full im1 ) ;
        im1 )
    else
      raise mismatchException
  end


  fun toString( im : image ) : string =
  let
    val ( height, width ) = dimensions im
  in
    String.concat( 
      List.foldr
        ( fn( y, strings ) => 
            "\n" :: ( 
              List.foldr
                ( fn( x, strings' ) => 
                    ( Spec.pixelToString( sub( im, x, y ) ) ^ ", " ) ::
                        strings' )
                strings
                ( List.tabulate( width, fn x => x ) ) ) )
        []
        ( List.tabulate( height, fn x => x ) ) )
  end


  fun transposed( im : image ) : image =
  let
    val ( height, width ) = dimensions im
    val out = zeroImage( height, width )
    val _ =
      appi
        ( fn( y, x, pix ) => update( out, x, y, pix ) )
        ( full im )
  in
    out
  end

  fun equal( im1 : image, im2 : image ) : bool = 
  let
    val ( height1, width1 ) = dimensions im1
    val ( height2, width2 ) = dimensions im2
  in
    if width1=width2 andalso height1=height2 then
      foldi RowMajor
        ( fn( y, x, pixel, eq ) => 
            eq andalso pixelEqual( pixel, sub( im2, y, x ) ) ) 
        true 
        ( full im1 )
    else
      false
  end

  fun fill( im : image, pix : pixel ) : unit =
    modify RowMajor ( fn _ => pix ) im


  local

    fun odd( x : int ) : bool = ( x mod 2 )=1

    fun filter( im : image, 
                mask : image, 
                extension : borderExtension,
                outputShape : outputSize, 
                loopMask : int * int * int * pixel -> pixel ) 
        : image =
    let

      val ( height, width ) = dimensions im
      val ( maskHeight, maskWidth ) = dimensions mask

      val output = createZero( width, height )
      val ( outputHeight, outputWidth ) = dimensions output

      val totalSize = width*height
      
      fun loop( index : int ) =
        case index<totalSize of 
          false => ()
        | true => ( 
          let
            val x = index mod width
            val y = index div width
            val sum = loopMask( x, y, 0, zeroPixel )
            val _ = update( output, y, x, sum )
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
                  ( im : image, mask : image ) 
        : image =
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

      fun loopMask( x : int, y : int, index : int, sum : pixel ) 
          : pixel =
        case index<maskTotal of
          false => sum
        | true => 
          let
            val xx = x+(index mod maskWidth-centerX)
            val yy = y+(index div maskWidth-centerY)
            val imageIndex = trunc( yy, height )*width+trunc( xx, width )
          in
            loopMask( 
              x, 
              y, 
              index+1, 
              pixelAdd( 
                sum, 
                pixelMul( sub( maskPixels, index ), sub( values, imageIndex ) ) ) )
          end 
    in
      filter( im, mask, extension, outputSize, loopMask )
    end


    (* 
    * Convolve an image with a two-dimensional mask 
    *)
    fun convolve ( extension : borderExtension, outputSize : outputSize )
                 ( im : image, mask : image ) 
        : image =
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

      fun loopMask( x : int, y : int, index : int, sum : pixel ) 
          : pixel =
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


  (*
     Rotate the image using bilinear interpolation
  *)
  fun rotateCrop( img : image, by : real, newWidth : int, newHeight : int) 
             : image =
  let
    val { width = width, height=height, ... } = img

    val newImage = zeroImage(newWidth, newHeight);
    
    
    fun calculateRotationY(dstX : int, dstY : int, u : real, v : real) : unit = 
    let
       val x = u * Math.cos(by) + v * Math.sin(by) + (real (width - 1)) / 2.0;
       val y = v * Math.cos(by) - u * Math.sin(by) + (real (height - 1)) / 2.0;
       
       val _ = if (x >= 0.0 andalso y >= 0.0) then
       let
          val x0 = Real.floor x
          val x1 = Real.ceil x
          val y0 = Real.floor y
          val y1 = Real.ceil y
          val _ = if (0 <= x0) andalso (x1 < width) andalso 
             (0 <= y0) andalso (y1 < height) then
          let

             val m00 = sub (img, x0, y0)
             val m01 = sub (img, x0, y1)
             val m10 = sub (img, x1, y0)
             val m11 = sub (img, x1, y1)
             
             val t0 = if (not(x0 = x1)) then 
                         Spec.pixelAdd(Spec.pixelScale(m00, (real x1) - x),
                         Spec.pixelScale(m10, x-(real x0))) else m00
             val t1 = if (not(x0 = x1)) then 
                         Spec.pixelAdd( Spec.pixelScale(m01, (real x1 - x)),
                         Spec.pixelScale(m11, x- (real x0))) else m01
             val ty = if (not(y0 = y1)) then 
                         Spec.pixelAdd( Spec.pixelScale(t0, (real y1) - y),
                         Spec.pixelScale(t1,  y - (real y0)))
                      else t0
            
             val _ = update (newImage, dstX, dstY, ty)
          in
             ()
          end
          else ()
       in 
          ()
       end
       else ()

    in
     if (dstY < newHeight -1) then calculateRotationY(dstX, dstY + 1, u, v + 1.0) 
     else () 
    end

    fun calculateRotationX(dstX : int, u : real) : unit =
       let
          val _ = calculateRotationY(dstX, 0, u, (~(real newHeight - 1.0) / 2.0))
       in
           if (dstX < newWidth) then 
              calculateRotationX(dstX + 1, u + 1.0) 
           else ()
       end

    val _ = calculateRotationX(0, (~((real newWidth - 1.0) / 2.0)));
  in
     newImage
  end

  fun rotate (img : image, by : real ) : image =
  let
    val { width = width, height=height, ... } = img
    val newWidth = Real.ceil(Real.max(
        abs((real width) * Math.cos(by) - (real height) * Math.sin(by)),
        abs((real width) * Math.cos(by) + (real height) * Math.sin(by))));
    val newHeight = Real.ceil(Real.max(
        abs((real width) * Math.sin(by) - (real height) * Math.cos(by)),
        abs((real width) * Math.sin(by) + (real height) * Math.cos(by))));
  in
     rotateCrop(img, by, newWidth, newHeight)
  end

end
