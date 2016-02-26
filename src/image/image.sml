(*
* file: image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains the image signature and the image type functor used
* to create new image types.
* 
* All images are represented using a normal array indexed in row-major fashion.
* Unlike Array2D, images use x (horizontal axis) and y (vertical axis) instead
* row i and column j. This seems more intuitive than using the same indexing 
* for images as for two dimensional arrays.
*)


(* 
* This signature specify all the mappings shared by all image types.
*)
signature IMAGE =
sig

  type 'a image 

  val image : int * int * 'a -> 'a image
  val zeroImage : int * int -> 'a image
  val fromList : int * int * 'a list -> 'a image
  val transposed : 'a image -> 'a image

  val load : string -> 'a image option
  val save': PNMCommon.format * word -> 'a image * string -> unit
  val save : 'a image * string -> unit

  val sub : 'a image * int * int -> 'a
  val sub' : 'a image * int -> 'a
  val update : 'a image * int * int * 'a -> unit
  val update': 'a image * int * 'a -> unit

  val add : 'a image * 'a image -> 'a image 
  val add' : 'a image * 'a image -> unit 
  val subtract : 'a image * 'a image -> 'a image
  val subtract' : 'a image * 'a image -> unit

  val app : ( 'a -> unit ) -> 'a image -> unit
  val appi : ( int * 'a -> unit ) -> 'a image -> unit
  val appxy : ( int * int * 'a -> unit ) -> 'a image -> unit
  val foldl : ( 'a * 'b -> 'b ) -> 'b -> 'a image -> 'b
  val foldli : ( int * 'a * 'b -> 'b ) -> 'b -> 'a image -> 'b
  val foldlxy : ( int * int * 'a * 'b -> 'b ) -> 'b -> 'a image -> 'b
  val foldr : ( 'a * 'b -> 'b ) -> 'b -> 'a image -> 'b
  val foldri : ( int * 'a * 'b -> 'b ) -> 'b -> 'a image -> 'b
  val foldrxy : ( int * int * 'a * 'b -> 'b ) -> 'b -> 'a image -> 'b
  val modify : ( 'a -> 'a ) -> 'a image -> unit
  val modifyi : ( int * 'a -> 'a ) -> 'a image -> unit
  val modifyxy : ( int * int * 'a -> 'a ) -> 'a image -> unit
  val tabulatexy : (int * int * (int * int -> 'a))  -> 'a image


  val fill : 'a image * 'a -> unit

  val correlate : ImageCommon.borderExtension * ImageCommon.outputSize -> 
                  'a image * 'a image -> 
                  'a image
  val convolve : ImageCommon.borderExtension * ImageCommon.outputSize -> 
                 'a image * 'a image -> 
                 'a image

  val thresholds' : ImageCommon.thresholdsMethod -> 'a image -> real list
  val thresholds : 'a image -> real list

  val equal : 'a image * 'a image -> bool

  val toString : 'a image -> string

  val rotate : ('a image * real) -> 'a image
  val rotateCrop : ('a image * real * int * int) -> 'a image

end


(* 
* This signature specify the interface for creating new image types.
*)
signature IMAGE_SPEC = 
sig
  
  type 'a image = { width : int, height : int, values : 'a Array.array }
  
  val depth : int
  val zeroPixel : 'a

  val pnmFormat : PNMCommon.format
  val pnmMaxVal : word

  val pixelAdd : 'a * 'a -> 'a
  val pixelSub : 'a * 'a -> 'a
  val pixelMul : 'a * 'a -> 'a
  val pixelMul' : 'a * real -> 'a
  val pixelEqual : 'a * 'a -> bool

  val pixelFromWords : word list * word * bool -> 'a
  val pixelToWords : 'a * word * bool -> word list

  val pixelToString : 'a -> string

end



(*
* This functor is used to create all the image types. The functor takes a 
* structure matching the IMAGE_SPEC signature as parameter.
*)
functor ImageFun( Spec : IMAGE_SPEC ) : IMAGE = 
struct

  open ImageCommon


  structure Spec = Spec


  type 'a image = 'a Spec.image

  
  val image = Array2D.array
  val fromList = Array2D.fromList

  fun zeroImage( width : int, height : int ) : 'a image = 
    image( width, height, Spec.zeroPixel )


  fun load( filename : string ) : 'a image option = 
    SOME( PNM.load filename )
    handle PNM.pnmException msg => (
      print( "Could not load image from " ^ filename ^ ": " ^ msg ^ "\n" );
      NONE )

  fun save' ( format : PNMCommon.format, maxVal : word )
            ( im : 'a image, filename : string ) 
      : unit = 
    PNM.save( im, filename, format, maxVal ) 
    handle PNM.pnmException msg =>
      print( "Could not save image to " ^ filename ^ ": " ^ msg ^ "\n" )
      
  fun save( im : 'a image, filename : string ) : unit = 
    save' ( Spec.pnmFormat, Spec.pnmMaxVal ) ( im, filename )


  fun sub( im as { width, values, ... } : 'a image, x : int, y : int ) 
      : 'a =
    Array.sub( values, y*width+x )

  fun sub'( im as { values, ... } : 'a image, index : int ) : 'a =
    Array.sub( values, index )


  fun update( im as { width, values, ... } : 'a image, 
              x : int, y : int, pix : 'a ) 
      : unit =
    Array.update( values, y*width+x, pix )

  fun update'( im as { width, values, ... } : 'a image, 
               index : int, pix : 'a ) 
      : unit =
    Array.update( values, index, pix )


  fun add( { width=width1, height=height1, values=values1 } : 'a image, 
           { width=width2, height=height2, values=values2 } : 'a image ) 
      : 'a image = 
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

  fun add'( { width=width1, height=height1, values=values1 } : 'a image, 
            { width=width2, height=height2, values=values2 } : 'a image )
      : unit = 
    if width1=width2 andalso height1=height2 then
      Array.modifyi 
        ( fn( i, x ) => 
            Spec.pixelAdd( x, Array.sub( values2, i ) ) )
        values1
    else
      raise mismatchException

  fun subtract( { width=width1, height=height1, values=values1 } : 'a image, 
                { width=width2, height=height2, values=values2 } : 'a image ) 
      : 'a image = 
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

  fun subtract'( { width=width1, height=height1, values=values1 } : 'a image, 
                 { width=width2, height=height2, values=values2 } : 'a image ) 
      : unit = 
    if width1=width2 andalso height1=height2 then
      Array.modifyi 
        ( fn( i, x ) => 
            Spec.pixelSub( x, Array.sub( values2, i ) ) )
        values1
    else
      raise mismatchException


  fun app ( f : 'a -> unit )
          ( im as { values, ... } : 'a image )
      : unit =
    Array.app f values

  fun appi ( f : int * 'a -> unit )
           ( im as { values, ... } : 'a image )
      : unit =
    Array.appi f values

  fun appxy ( f : int * int * 'a -> unit )
            ( im as { width, values, ... } : 'a image )
      : unit =
    Array.appi ( fn( i, pix ) => f( i mod width, i div width, pix ) ) values

  fun foldl ( f : 'a * 'a -> 'a )
            ( start : 'a )
            ( im as { values, ... } : 'a image ) : 'a =
    Array.foldl f start values

  fun foldli ( f : int * 'a * 'a -> 'a )
             ( start : 'a )
             ( im as { values, ... } : 'a image ) : 'a =
    Array.foldli f start values

  fun foldlxy ( f : int * int * 'a * 'a -> 'a )
             ( start : 'a )
             ( im as { width, values, ... } : 'a image ) : 'a =
    Array.foldli 
      ( fn( i, pix, x ) => f( i mod width, i div width, pix, x ) ) 
      start 
      values

  fun foldr ( f : 'a * 'a -> 'a )
            ( start : 'a )
            ( im as { values, ... } : 'a image ) : 'a =
    Array.foldr f start values

  fun foldri ( f : int * 'a * 'a -> 'a )
             ( start : 'a )
             ( im as { values, ... } : 'a image ) : 'a =
    Array.foldri f start values

  fun foldrxy ( f : int * int * 'a * 'a -> 'a )
             ( start : 'a )
             ( im as { width, values, ... } : 'a image ) : 'a =
    Array.foldri 
      ( fn( i, pix, x ) => f( i mod width, i div width, pix, x ) ) 
      start 
      values

  fun modify ( f : 'a -> 'a )
             ( im as { values, ... } : 'a image ) 
      : unit =
    Array.modify f values

  fun modifyi ( f : int * 'a -> 'a )
             ( im as { values, ... } : 'a image ) 
      : unit =
    Array.modifyi f values

  fun modifyxy ( f : int * int * 'a -> 'a )
             ( im as { width, values, ... } : 'a image ) 
      : unit =
    Array.modifyi 
      ( fn( i, pix ) => f( i mod width, i div width, pix ) ) 
      values

  fun tabulatexy (width : int, height : int, f : int * int -> 'a ) : 'a image =
    let
       val img = zeroImage(width, height);
       val _ = modifyxy (fn (x,y,_) => f(x,y) ) img
    in
       img
    end
     

  fun toString( im as { width, height, ... } : 'a image ) : string =
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


  fun transposed( im as { width, height, ... } : 'a image ) : 'a image =
  let
    val out = zeroImage( height, width )
    val _ =
      appxy
        ( fn( x, y, pix ) => update( out, y, x, pix ) )
        im
  in
    out
  end

  fun equal( im1 : 'a image, im2 : 'a image ) : bool = 
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

  fun fill( im : 'a image, pix : 'a ) : unit =
    modify ( fn _ => pix ) im


  structure FilterImage : FILTER_IMAGE = 
  struct
    
    type 'a image = Spec.image

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
  fun rotateCrop( img : 'a image, by : real, newWidth : int, newHeight : int) 
             : 'a image =
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

  fun rotate (img : 'a image, by : real ) : 'a image =
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
