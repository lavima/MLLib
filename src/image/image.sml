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
    WrapExtension |
    MirrorExtension

  datatype outputSize = 
    OriginalSize | 
    FullSize

  type pixel
  type image 
  type region

  exception mismatchException

  val region : image * int * int * int option * int option -> region
  val full : image -> region

  val image : int * int * pixel -> image
  val zeroImage : int * int -> image

  val dimensions : image -> int * int
  val nRows : image -> int
  val nCols : image -> int

  val row : image * int -> pixel Vector.vector
  val column : image * int -> pixel Vector.vector

  val fromList : pixel list list -> image
  val fromList' : int * int * pixel list -> image

  val copy : { src : region, dst : image, dst_row : int, dst_col : int } -> unit

  val transposed : image -> image

  val sub : image * int * int -> pixel
  val update : image * int * int * pixel -> unit

  val add : image * image -> image 
  val add' : image * image -> unit 
  val subtract : image * image -> image
  val subtract' : image * image -> unit

  val scale : image * real -> image
  val scale' : image * real -> unit

  val app : traversal -> ( pixel -> unit ) -> image -> unit
  val appi : traversal -> ( int * int * pixel -> unit ) -> region -> unit
  val fold : traversal -> ( pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldi : 
    traversal -> ( int * int * pixel * 'a -> 'a ) -> 'a -> region -> 'a
  val modify : traversal -> ( pixel -> pixel ) -> image -> unit
  val modifyi : traversal -> ( int * int * pixel -> pixel ) -> region -> unit
  val tabulate : traversal -> ( int * int * ( int * int -> pixel ) )  -> image

  val listFoldl : 
    traversal -> ( pixel * pixel -> pixel ) -> image -> image list -> image
  val listFoldr : 
    traversal -> ( pixel * pixel -> pixel ) -> image -> image list -> image

  val fill : image * pixel -> unit

  val correlate : borderExtension * outputSize -> image * image -> image
  val convolve : borderExtension * outputSize -> image * image -> image

  val equal : image * image -> bool

  val toString : image -> string

  val rotate : (image * real) -> image
  val rotateCrop : (image * real * int * int) -> image

  val border : borderExtension * int -> image -> image
  val trim : int -> image -> image

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
    WrapExtension |
    MirrorExtension

  datatype outputSize = 
    OriginalSize | 
    FullSize

  type image = pixel array
  type region = pixel region
  
  exception mismatchException

  fun region( im : image, 
              row : int, col : int, 
              nrows : int option, ncols : int option ) 
      : region =
    { base=im, row=row, col=col, nrows=nrows, ncols=ncols }

  fun full( im : image ) : region =
    { base=im, row=0, col=0, nrows=NONE, ncols=NONE }

  fun image( rows : int, cols : int, x : pixel ) : image = 
    Array2.array( rows, cols, x )

  fun fromList'( rows : int, cols : int, pixels : pixel list ) : image =
  let
    fun build( xs : pixel list ) : pixel list list =
      case xs of 
        [] => []
      | _::_ =>
          List.take( xs, cols )::build( List.drop( xs, cols ) )

    val _ = 
      case rows*cols=List.length pixels of 
        false => raise Size
      | true => ()
  in
    fromList( build pixels )
  end

  fun zeroImage( rows : int, cols : int ) : image = 
    image( rows, cols, zeroPixel )

  fun add( im1 : image, im2 : image ) : image = 
  let
    val ( height1, width1 ) = dimensions im1
    val ( height2, width2 ) = dimensions im2
  in
    if width1=width2 andalso height1=height2 then
    let
      val output = zeroImage( height1, width1 )
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

  fun add'( im1 : image, im2 : image ) : unit = 
  let
    val ( height1, width1 ) = dimensions im1
    val ( height2, width2 ) = dimensions im2
  in
    if width1=width2 andalso height1=height2 then
      ( modifyi RowMajor 
          ( fn( i, j, p1 ) => pixelAdd( p1, sub( im2, i, j ) ) )
          ( full im1 ) )
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
      val output = zeroImage( height1, width1 )
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

  fun subtract'( im1 : image, im2 : image ) : unit = 
  let
    val ( height1, width1 ) = dimensions im1
    val ( height2, width2 ) = dimensions im2
  in
    if width1=width2 andalso height1=height2 then
      ( modifyi RowMajor
          ( fn( i, j, p1 ) => pixelSub( p1, sub( im2, i, j ) ) )
          ( full im1 ) )
    else
      raise mismatchException
  end

  fun scale( im : image, scalar : real ) : image = 
  let
    val ( height, width ) = dimensions im
    val output = tabulate RowMajor 
      ( height, width, fn ( i, j ) => pixelScale( sub( im, i, j ), scalar ) )
  in
    output
  end

  fun scale'( im : image, scalar : real ) : unit = 
  let
    val ( height, width ) = dimensions im
    val _ = modifyi RowMajor ( fn ( i, j, p ) => pixelScale( p, scalar ) )
      ( full im )
  in
    ()
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
                    ( Spec.pixelToString( sub( im, y, x ) ) ^ ", " ) ::
                        strings' )
                strings
                ( List.tabulate( width, fn x => x ) ) ) )
        []
        ( List.tabulate( height, fn x => x ) ) )
  end


  fun transposed( im : image ) : image =
  let
    val ( height, width ) = dimensions im
    val out = zeroImage( width, height )
    val _ =
      appi RowMajor
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

  fun listFoldl ( tr : traversal )
                ( f : pixel * pixel -> pixel )
                ( start : image )
                ( is : image list )
      : image =
    List.foldl 
      ( fn( im, out ) => (
          modifyi tr
            ( fn( i, j, x ) => f( sub( im, i, j ), x ) )
            ( full out ) ;
          out ) )
      start
      is
          
  fun listFoldr ( tr : traversal )
                ( f : pixel * pixel -> pixel )
                ( start : image )
                ( is : image list )
      : image =
    List.foldr 
      ( fn( im, out ) => (
          modifyi tr
            ( fn( i, j, x ) => f( sub( im, i, j ), x ) )
            ( full out ) ;
          out ) )
      start
      is
          

  fun fill( im : image, pix : pixel ) : unit =
    modify RowMajor ( fn _ => pix ) im


  local

    fun odd( x : int ) : bool = ( x mod 2 )=1

    fun getValue( im : image, extension : borderExtension ) 
                ( x : int, y : int ) 
        : pixel =
    let
      val ( height, width ) = dimensions im

      fun zero( v : int, max : int ) : int = 
        if v>=0 andalso v<max then 
          v
        else if v<0 then 
          0
        else
          max-1

      fun copy( v : int, max : int ) : int = 
        if v>=0 andalso v<max then 
          v
        else if v<0 then 
          0
        else
          max-1

      fun wrap( v : int, max : int ) : int =
        if v>=0 andalso v<max then 
          v
        else if v<0 then 
          max+( v mod ~max )
        else
          v mod max

      fun mirror( v : int, max : int ) : int =
        if v>=0 andalso v<max then 
          v
        else if v<0 then 
          ~( v mod ~max )
        else
          max-( ( v mod max ) )

      val value =
        case extension of 
          ZeroExtension => 
            if x>=0 andalso x<width andalso y>=0 andalso y<height then 
              sub( im, y, x )
            else
              zeroPixel
        | CopyExtension => sub( im, copy( y, height ), copy( x, width ) )
        | WrapExtension => sub( im, wrap( y, height ), wrap( x, width ) ) 
        | MirrorExtension => 
          sub( im, mirror( y, height-1 ), mirror( x, width-1 ) )
      
    in
      value
    end

  in (* local *)

    fun correlate ( extension : borderExtension, outputSize : outputSize )
                  ( im : image, mask : image ) 
        : image =
    let

      val ( height, width ) = dimensions im
      val ( maskHeight, maskWidth ) = dimensions mask

      val out = zeroImage( height, width )
      val ( outHeight, outWidth ) = dimensions out
      
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

      val ( left, right, top, bottom ) = 
        case outputSize of
          OriginalSize => ( 0, width-1, 0, height-1 )
        | FullSize => 
            ( centerX, outWidth-( maskWidth-centerX ), 
              centerY, outHeight-( maskHeight-centerY ) )

      val getValue = getValue ( im, extension )

      val _ = 
        modifyi RowMajor
          ( fn( oy, ox, _ ) =>
              foldi RowMajor
                ( fn( my, mx, mv, sum ) =>
                    pixelAdd( 
                      sum,
                      pixelMul( 
                        mv, 
                        getValue( 
                          ( ox+( mx-centerX ) )-left, 
                          ( oy+( my-centerY ) )-top ) ) ) )
                zeroPixel
                ( if ox<left andalso oy<top then
                    region( mask, oy, ox, NONE, NONE ) 
                  else if ox>right andalso oy<top then
                    region(
                      mask,
                      oy, 0,
                      NONE, SOME( maskWidth-( ox-right ) ) ) 
                  else if ox>right andalso oy>bottom then
                    region( 
                      mask, 
                      0, 0, 
                      SOME( maskHeight-( oy-bottom ) ), 
                      SOME( maskWidth-( ox-right ) ) ) 
                  else if ox<left andalso oy>bottom then
                    region(
                      mask,
                      0, ox,
                      SOME( maskHeight-( oy-bottom ) ), NONE )
                  else
                    full mask ) )
          ( full out )

    in
      out
    end

    (* 
    * Convolve an image with a two-dimensional mask 
    *)
    fun convolve ( extension : borderExtension, outputSize : outputSize )
                 ( im : image, mask : image ) 
        : image =
    let

      val ( height, width ) = dimensions im
      val ( maskHeight, maskWidth ) = dimensions mask

      val out = zeroImage( height, width )
      val ( outHeight, outWidth ) = dimensions out
      
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

      val ( left, right, top, bottom ) = 
        case outputSize of
          OriginalSize => ( 0, width-1, 0, height-1 )
        | FullSize => 
            ( centerX, outWidth-( maskWidth-centerX ), 
              centerY, outHeight-( maskHeight-centerY ) )

      val getValue = getValue ( im, extension )

      val _ = 
        modifyi RowMajor
          ( fn( oy, ox, _ ) =>
              foldi RowMajor
                ( fn( my, mx, mv, sum ) =>
                    pixelAdd( 
                      sum,
                      pixelMul( 
                        mv, 
                        getValue( 
                          ( ox+( ( maskWidth-mx-1 )-centerX ) )-left, 
                          ( oy+( ( maskHeight-my-1 )-centerY ) )-top ) ) ) )
                zeroPixel
                ( if ox<left andalso oy<top then
                    region( mask, oy, ox, NONE, NONE ) 
                  else if ox>right andalso oy<top then
                    region(
                      mask,
                      oy, 0,
                      NONE, SOME( maskWidth-( ox-right ) ) ) 
                  else if ox>right andalso oy>bottom then
                    region( 
                      mask, 
                      0, 0, 
                      SOME( maskHeight-( oy-bottom ) ), 
                      SOME( maskWidth-( ox-right ) ) ) 
                  else if ox<left andalso oy>bottom then
                    region(
                      mask,
                      0, ox,
                      SOME( maskHeight-( oy-bottom ) ), NONE )
                  else
                    full mask ) )
          ( full out )

    in
      out
    end

  fun border( borderExtension : borderExtension, border : int ) 
            ( img : image ) 
    : image =
  let
    val ( height, width ) = dimensions img

    val getValue = getValue ( img, borderExtension )

    val newImg = tabulate RowMajor ( 
      height + 2 * border, width + 2 * border, 
      fn ( y, x ) => getValue( x-border, y-border ) )
  in
    newImg
  end


  end (* local *)


  (*
     Rotate the image using bilinear interpolation
  *)
  fun rotateCrop( img : image, by : real, newHeight : int, newWidth : int ) 
      : image =
  let
    val ( height, width ) = dimensions img

    val newImage = zeroImage( newHeight, newWidth )
    
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
          val _ = if (0 <= x0) andalso 
                     (x1 < width) andalso 
                     (0 <= y0) andalso 
                     (y1 < height) then
          let

             val m00 = sub (img, y0, x0)
             val m01 = sub (img, y1, x0)
             val m10 = sub (img, y0, x1)
             val m11 = sub (img, y1, x1)
             
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
            
             val _ = update (newImage, dstY, dstX, ty)
          in
             ()
          end
          else ()
       in 
          ()
       end
       else ()

    in
     if (dstY < newHeight -1) then 
       calculateRotationY(dstX, dstY + 1, u, v + 1.0) 
     else () 
    end

    fun calculateRotationX(dstX : int, u : real) : unit =
      let
        val _ = calculateRotationY(dstX, 0, u, (~(real newHeight - 1.0) / 2.0))
      in
        if (dstX < newWidth - 1) then 
          calculateRotationX(dstX + 1, u + 1.0) 
        else ()
      end

    val _ = calculateRotationX(0, (~((real newWidth - 1.0) / 2.0)));
  in
     newImage
  end

  fun rotate (img : image, by : real ) : image =
  let
    val ( height, width ) = dimensions img
    val newWidth = Real.ceil(Real.max(
        abs((real width) * Math.cos(by) - (real height) * Math.sin(by)),
        abs((real width) * Math.cos(by) + (real height) * Math.sin(by))));
    val newHeight = Real.ceil(Real.max(
        abs((real width) * Math.sin(by) - (real height) * Math.cos(by)),
        abs((real width) * Math.sin(by) + (real height) * Math.cos(by))));
  in
     rotateCrop( img, by, newHeight, newWidth )
  end

  fun trim ( border : int ) ( img : image ) : image =
  let
    val ( height, width ) = dimensions img
    val newImg = 
      tabulate RowMajor ( 
        height - 2 * border, width - 2 * border,
        fn( y, x ) => sub( img, y + border, x + border ) )
  in
    newImg
  end

end
