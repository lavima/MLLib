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

  type element
  type pixel
  type image 


  val image : int * int * pixel -> image
  val zeroImage : int * int -> image
  val fromList : int * int * pixel list -> image
  val transposed : image -> image

  val load : string -> image option
  val save': PNMCommon.format * word -> image * string -> unit
  val save : image * string -> unit

  val sub : image * int * int -> pixel
  val sub' : image * int -> pixel
  val update : image * int * int * pixel -> unit
  val update': image * int * pixel -> unit

  val add : image * image -> image 
  val add' : image * image -> unit 
  val subtract : image * image -> image
  val subtract' : image * image -> unit

  val app : ( pixel -> unit ) -> image -> unit
  val appi : ( int * pixel -> unit ) -> image -> unit
  val appxy : ( int * int * pixel -> unit ) -> image -> unit
  val foldl : ( pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldli : ( int * pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldlxy : ( int * int * pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldr : ( pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldri : ( int * pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val foldrxy : ( int * int * pixel * 'a -> 'a ) -> 'a -> image -> 'a
  val modify : ( pixel -> pixel ) -> image -> unit
  val modifyi : ( int * pixel -> pixel ) -> image -> unit
  val modifyxy : ( int * int * pixel -> pixel ) -> image -> unit
  val tabulatexy : (int * int * (int * int -> pixel))  -> image


  val fill : image * pixel -> unit

  val correlate : ImageCommon.borderExtension * ImageCommon.outputSize -> 
                  image * image -> 
                  image
  val convolve : ImageCommon.borderExtension * ImageCommon.outputSize -> 
                 image * image -> 
                 image

  val histograms : image * int -> int Array.array list

  val thresholds' : ImageCommon.thresholdsMethod -> image -> real list
  val thresholds : image -> real list

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
  
  type element
  type pixel 
  type image = { Width : int, Height : int, Values : pixel Array.array }
  
  val Depth : int
  val ZeroPixel : pixel

  val PNMFormat : PNMCommon.format
  val PNMMaxVal : word

  val pixelAdd : pixel * pixel -> pixel
  val pixelSub : pixel * pixel -> pixel
  val pixelMul : pixel * pixel -> pixel
  val pixelMul' : pixel * real -> pixel
  val pixelEqual : pixel * pixel -> bool

  val getElement : pixel * int -> element
  val elementFromReal : real -> element
  val elementCompare : element * element -> order

  val pixelFromWords : word list * word * bool -> pixel
  val pixelToWords : pixel * word * bool -> word list

  val pixelToString : pixel -> string

end



(*
* This functor is used to create all the image types. The functor takes a 
* structure matching the IMAGE_SPEC signature as parameter.
*)
functor ImageFun( Spec : IMAGE_SPEC ) : IMAGE = 
struct

  open ImageCommon


  structure Spec = Spec


  type element = Spec.element
  type pixel = Spec.pixel
  type image = Spec.image

  
  val image = Array2D.array
  val fromList = Array2D.fromList

  fun zeroImage( Width : int, Height : int ) : image = 
    image( Width, Height, Spec.ZeroPixel )


  structure PNMImage : PNM_IMAGE =
  struct

    exception pnmImageException of string

    type element = Spec.element
    type image = Spec.image 
    type pixel = Spec.pixel

    val pixelFromWords = Spec.pixelFromWords
    val pixelToWords = Spec.pixelToWords

    val createImage = fromList

    val Format = Spec.PNMFormat
    val Depth = Spec.Depth

  end (* structure PNMImage *)


  structure PNM = PNMFun( PNMImage )


  fun load( Filename : string ) : image option = 
    SOME( PNM.load Filename )
    handle PNM.pnmException Msg => (
      print( "Could not load image from " ^ Filename ^ ": " ^ Msg ^ "\n" );
      NONE )

  fun save' ( Format : PNMCommon.format, MaxVal : word )
            ( Image : image, Filename : string ) 
      : unit = 
    PNM.save( Image, Filename, Format, MaxVal ) 
    handle PNM.pnmException Msg =>
      print( "Could not save image to " ^ Filename ^ ": " ^ Msg ^ "\n" )
      
  fun save( Image : image, Filename : string ) : unit = 
    save' ( Spec.PNMFormat, Spec.PNMMaxVal ) ( Image, Filename )


  fun sub( Image as { Width, Values, ... } : image, X : int, Y : int ) 
      : pixel =
    Array.sub( Values, Y*Width+X )

  fun sub'( Image as { Values, ... } : image, Index : int ) : pixel =
    Array.sub( Values, Index )


  fun update( Image as { Width, Values, ... } : image, 
              X : int, Y : int, Pixel : pixel ) 
      : unit =
    Array.update( Values, Y*Width+X, Pixel )

  fun update'( Image as { Width, Values, ... } : image, 
               Index : int, Pixel : pixel ) 
      : unit =
    Array.update( Values, Index, Pixel )


  fun add( Image1 as { Width=Width1, Height=Height1, Values=Values1 } : image, 
           Image2 as { Width=Width2, Height=Height2, Values=Values2 } : image ) 
      : image = 
    if Width1=Width2 andalso Height1=Height2 then
    let
      val Output as { Values=OutputValues, ... } = zeroImage( Width1, Height1 )
      val _ = 
        Array.modifyi 
          ( fn( I, _ ) => 
              Spec.pixelAdd( 
                Array.sub( Values1, I ), 
                Array.sub( Values2, I ) ) )
          OutputValues
    in
      Output
    end
    else
      raise mismatchException

  fun add'( Image1 as { Width=Width1, Height=Height1, Values=Values1 } : image, 
            Image2 as { Width=Width2, Height=Height2, Values=Values2 } : image )
      : unit = 
    if Width1=Width2 andalso Height1=Height2 then
      Array.modifyi 
        ( fn( I, X ) => 
            Spec.pixelAdd( X, Array.sub( Values2, I ) ) )
        Values1
    else
      raise mismatchException

  fun subtract( Image1 as { Width=Width1, Height=Height1, Values=Values1 } : image, 
                Image2 as { Width=Width2, Height=Height2, Values=Values2 } : image ) 
      : image = 
    if Width1=Width2 andalso Height1=Height2 then
    let
      val Output as { Values=OutputValues, ... } = zeroImage( Width1, Height1 )
      val _ = 
        Array.modifyi 
          ( fn( I, _ ) => 
              Spec.pixelSub( 
                Array.sub( Values1, I ), 
                Array.sub( Values2, I ) ) )
          OutputValues
    in
      Output
    end
    else
      raise mismatchException

  fun subtract'( Image1 as { Width=Width1, Height=Height1, Values=Values1 } : image, 
                 Image2 as { Width=Width2, Height=Height2, Values=Values2 } : image ) 
      : unit = 
    if Width1=Width2 andalso Height1=Height2 then
      Array.modifyi 
        ( fn( I, X ) => 
            Spec.pixelSub( X, Array.sub( Values2, I ) ) )
        Values1
    else
      raise mismatchException


  fun app ( f : pixel -> unit )
          ( Image as { Values, ... } : image )
      : unit =
    Array.app f Values

  fun appi ( f : int * pixel -> unit )
           ( Image as { Values, ... } : image )
      : unit =
    Array.appi f Values

  fun appxy ( f : int * int * pixel -> unit )
            ( Image as { Width, Values, ... } : image )
      : unit =
    Array.appi ( fn( I, Pixel ) => f( I mod Width, I div Width, Pixel ) ) Values

  fun foldl ( f : pixel * 'a -> 'a )
            ( Start : 'a )
            ( Image as { Values, ... } : image ) : 'a =
    Array.foldl f Start Values

  fun foldli ( f : int * pixel * 'a -> 'a )
             ( Start : 'a )
             ( Image as { Values, ... } : image ) : 'a =
    Array.foldli f Start Values

  fun foldlxy ( f : int * int * pixel * 'a -> 'a )
             ( Start : 'a )
             ( Image as { Width, Values, ... } : image ) : 'a =
    Array.foldli 
      ( fn( I, Pixel, X ) => f( I mod Width, I div Width, Pixel, X ) ) 
      Start 
      Values

  fun foldr ( f : pixel * 'a -> 'a )
            ( Start : 'a )
            ( Image as { Values, ... } : image ) : 'a =
    Array.foldr f Start Values

  fun foldri ( f : int * pixel * 'a -> 'a )
             ( Start : 'a )
             ( Image as { Values, ... } : image ) : 'a =
    Array.foldri f Start Values

  fun foldrxy ( f : int * int * pixel * 'a -> 'a )
             ( Start : 'a )
             ( Image as { Width, Values, ... } : image ) : 'a =
    Array.foldri 
      ( fn( I, Pixel, X ) => f( I mod Width, I div Width, Pixel, X ) ) 
      Start 
      Values

  fun modify ( f : pixel -> pixel )
             ( Image as { Values, ... } : image ) 
      : unit =
    Array.modify f Values

  fun modifyi ( f : int * pixel -> pixel )
             ( Image as { Values, ... } : image ) 
      : unit =
    Array.modifyi f Values

  fun modifyxy ( f : int * int * pixel -> pixel )
             ( Image as { Width, Values, ... } : image ) 
      : unit =
    Array.modifyi 
      ( fn( I, Pixel ) => f( I mod Width, I div Width, Pixel ) ) 
      Values

  fun tabulatexy (width : int, height : int, f : int * int -> pixel) : image =
    let
       val img = zeroImage(width, height);
       val _ = modifyxy (fn (x,y,_) => f(x,y) ) img
    in
       img
    end
     


  fun toString( Image as { Width, Height, ... } : image ) : string =
    String.concat( 
      List.foldr
        ( fn( Y, Strings ) => 
            "\n" :: ( 
              List.foldr
                ( fn( X, Strings' ) => 
                    ( Spec.pixelToString( sub( Image, X, Y ) ) ^ ", " ) ::
                        Strings' )
                Strings
                ( List.tabulate( Width, fn X => X ) ) ) )
        []
        ( List.tabulate( Height, fn X => X ) ) )


  fun histograms( Image : image, NumBins : int ) 
      : int Array.array list = 
  let

    val Factor = 1.0/( ( real NumBins )-1.0 )

    val Delims = 
      List.tabulate( 
        NumBins+1, 
        fn X => Spec.elementFromReal( ( ( real X )-0.5 )*Factor ) )
 
    (*
    * Get the index of the histogram bin to place an element. 
    *)
    fun getIndex( Delims : element list, Element : element, Index : int )
        : int =
      case Delims of 
        Low::( RDelims as High::_ ) =>
          if not ( Spec.elementCompare( Element, Low )=LESS ) andalso
             Spec.elementCompare( Element, High )=LESS then
            Index
          else
            getIndex( RDelims, Element, Index+1 )
      | _ => raise Overflow
          
    fun collect( Channel : int ) : int Array.array list = 
      case Channel<Spec.Depth of
        false => []
      | true => 
        let
          val Histogram = Array.array( NumBins, 0 )
          val _ = appxy
            ( fn( X, Y, Pixel ) => 
              let
                val Index = 
                  getIndex( Delims, Spec.getElement( Pixel, Channel ), 0 )
                val Count = Array.sub( Histogram, Index )
              in
                Array.update( Histogram, Index, Count+1 ) 
              end )
            Image
        in
          Histogram::collect( Channel+1 )
        end

  in
    collect 0 
  end


  fun transposed( Image as { Width, Height, ... } : image ) : image =
  let
    val Out = zeroImage( Height, Width )
    val _ =
      appxy
        ( fn( X, Y, Pixel ) => update( Out, Y, X, Pixel ) )
        Image
  in
    Out
  end

  fun equal( Image1 : image, Image2 : image ) : bool = 
  let
    val { Width=Width1, Height=Height1, ... } = Image1
    val { Width=Width2, Height=Height2, ... } = Image2

    val Num = Width1*Height1

    fun check( Index : int ) : bool =
    case Index<Num of
      false => true
    | true =>
      let
        val Pixel1 = sub'( Image1, Index )
        val Pixel2 = sub'( Image2, Index )
      in
        if Spec.pixelEqual( Pixel1, Pixel2 ) then
          check( Index+1 )
        else
          false
      end
  in
    if Width1=Width2 andalso Height1=Height2 then
      check 0
    else
      false
  end

  fun fill( Image : image, Pixel : pixel ) : unit =
    modify ( fn _ => Pixel ) Image


  structure FilterImage : FILTER_IMAGE = 
  struct
    
    type pixel = Spec.pixel
    type image = Spec.image

    val ZeroPixel = Spec.ZeroPixel
    val createZero = zeroImage

    val pixelAdd = Spec.pixelAdd
    val pixelMul = Spec.pixelMul

  end (* structure FilterImage *)

  structure Filter = FilterFun( FilterImage )

  val correlate = Filter.correlate
  val convolve = Filter.convolve


  structure ThresholdsImage : THRESHOLDS_IMAGE =
  struct

    type pixel = Spec.pixel
    type image = Spec.image

    val histograms = histograms

  end (* structure ThresholdsImage *)

  structure Thresholds = ThresholdsFun( ThresholdsImage )


  fun thresholds' ( Method : thresholdsMethod ) 
                  ( Image : image )
      : real list =
    case Method of
      percentage( NumBins, Percentage ) =>
        Thresholds.percentage( Image, NumBins, Percentage )
    | otsu NumBins =>
        Thresholds.otsu( Image, NumBins )

  val thresholds : image -> real list = thresholds' ( otsu 256 )

  
  (*
     Rotate the image using bilinear interpolation
  *)
  fun rotateCrop(img : image, by : real, newWidth : int, newHeight : int) 
             : image =
  let
    val { Width = width, Height=height, ... } = img

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
    val { Width = width, Height=height, ... } = img
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
