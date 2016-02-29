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

  type pixel
  type image 

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
  val foldl : ( pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldli : ( int * pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldr : ( pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldri : ( int * pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val modify : ( pixel -> pixel ) -> image -> unit
  val modifyi : ( int * pixel -> pixel ) -> image -> unit
  val tabulate : (int * int * (int * int -> pixel))  -> image


  val fill : image * pixel -> unit

  val correlate : ImageCommon.borderExtension * ImageCommon.outputSize -> 
                  image * image -> 
                  image
  val convolve : ImageCommon.borderExtension * ImageCommon.outputSize -> 
                 image * image -> 
                 image

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
  val pixelMul' : pixel * real -> pixel
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
  open ImageCommon
  open Spec

  type image = pixel Array2.array
  
  val image = Array2.array

  fun fromList'( width : int, height : int, pixels : pixel list ) : image =
  let
    fun build( xs : pixel list ) : pixel list list =
      List.take( xs, width )::build( List.drop( xs, width ) )
  in
    fromList( width, height, build pixels )
  end

  fun zeroImage( width : int, height : int ) : image = 
    image( width, height, Spec.zeroPixel )

  fun add( { width=width1, height=height1, values=values1 } : image, 
           { width=width2, height=height2, values=values2 } : image ) 
      : image = 
    if width1=width2 andalso height1=height2 then
    let
      val output as { values=outputValues, ... } = zeroImage( width1, height1 )
      val _ = 
        Array.modifyi 
          ( fn( i, _ ) => 
              Spec.pixelAdd( 
                Array.sub( values1, i ), 
                Array.sub( values2, i ) ) )
          outputValues
    in
      output
    end
    else
      raise mismatchException

  fun add'( { width=width1, height=height1, values=values1 } : image, 
            { width=width2, height=height2, values=values2 } : image )
      : unit = 
    if width1=width2 andalso height1=height2 then
      Array.modifyi 
        ( fn( i, x ) => 
            Spec.pixelAdd( x, Array.sub( values2, i ) ) )
        values1
    else
      raise mismatchException

  fun subtract( { width=width1, height=height1, values=values1 } : image, 
                { width=width2, height=height2, values=values2 } : image ) 
      : image = 
    if width1=width2 andalso height1=height2 then
    let
      val output as { values=outputValues, ... } = zeroImage( width1, height1 )
      val _ = 
        Array.modifyi 
          ( fn( i, _ ) => 
              Spec.pixelSub( 
                Array.sub( values1, i ), 
                Array.sub( values2, i ) ) )
          outputValues
    in
      output
    end
    else
      raise mismatchException

  fun subtract'( { width=width1, height=height1, values=values1 } : image, 
                 { width=width2, height=height2, values=values2 } : image ) 
      : unit = 
    if width1=width2 andalso height1=height2 then
      Array.modifyi 
        ( fn( i, x ) => 
            Spec.pixelSub( x, Array.sub( values2, i ) ) )
        values1
    else
      raise mismatchException


  fun toString( im as { width, height, ... } : image ) : string =
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


  fun transposed( im as { width, height, ... } : image ) : image =
  let
    val out = zeroImage( height, width )
    val _ =
      appxy
        ( fn( x, y, pix ) => update( out, y, x, pix ) )
        im
  in
    out
  end

  fun equal( im1 : image, im2 : image ) : bool = 
  let
    val { width=width1, height=height1, ... } = im1
    val { width=width2, height=height2, ... } = im2

    val num = width1*height1

    fun check( index : int ) : bool =
    case index<num of
      false => true
    | true =>
      let
        val pix1 = sub'( im1, index )
        val pix2 = sub'( im2, index )
      in
        if Spec.pixelEqual( pix1, pix2 ) then
          check( index+1 )
        else
          false
      end
  in
    if width1=width2 andalso height1=height2 then
      check 0
    else
      false
  end

  fun fill( im : image, pix : pixel ) : unit =
    modify ( fn _ => pix ) im


  structure FilterImage : FILTER_IMAGE = 
  struct
    
    type image = Spec.image

    val zeroPixel = Spec.zeroPixel
    val createZero = zeroImage

    val pixelAdd = Spec.pixelAdd
    val pixelMul = Spec.pixelMul

  end (* structure FilterImage *)

  structure Filter = FilterFun( FilterImage )

  val correlate = Filter.correlate
  val convolve = Filter.convolve


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
                         Spec.pixelAdd(Spec.pixelMul'(m00, (real x1) - x),
                         Spec.pixelMul'(m10, x-(real x0))) else m00
             val t1 = if (not(x0 = x1)) then 
                         Spec.pixelAdd( Spec.pixelMul'(m01, (real x1 - x)),
                         Spec.pixelMul'(m11, x- (real x0))) else m01
             val ty = if (not(y0 = y1)) then 
                         Spec.pixelAdd( Spec.pixelMul'(t0, (real y1) - y),
                         Spec.pixelMul'(t1,  y - (real y0)))
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
