(*
* file: gradient_disk.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for calculating gradients using 
* circular disks.
* 
*)


  fun buildOrientationSliceMap( height : int, width : int, nori : int )
    : int Array2.array =
  let
    val sliceMap = Array2.array( height, width, 0 )

    val _ = Array2.modifyi Array2.RowMajor 
    ( fn ( y, x, _ ) => 
      let
        val dy = ( real y )-( real height )/2.0
        val dx = ( real x )-( real width )/2.0
        val ori = Math.atan2( dx, dy )+Math.pi
        val index = Real.floor( ori/Math.pi*( real nori ) )
      in
        if index >= 2*nori then 2*nori-1
        else index
      end )
    { base=sliceMap, row=0, col=0, nrows=NONE, ncols=NONE }
  in
    sliceMap
  end

structure GradientDisk : GRADIENT =
struct

  (* Must be rewritten to avoid copying into grayscale image,
     only used to eliminate potential error sources during development. *)
  fun convolve( data : real array, filter : RealGrayscaleImage.image ) =
  let
    val dataImage = RealGrayscaleImage.tabulate RealGrayscaleImage.RowMajor 
      (1,  Array.length data, 
     fn (i, j) => Array.sub(data, j) )
 
    val convolved = RealGrayscaleImage.convolve 
                    (RealGrayscaleImage.ZeroExtension, 
                     RealGrayscaleImage.OriginalSize )
                    ( dataImage, filter )
  in
    Array.modifyi ( fn (i, p) => RealGrayscaleImage.sub(convolved, 0, i) ) data
  end

  fun buildCircleImage( radius : int, fill : int ) 
    : IntGrayscaleImage.image =
    IntGrayscaleImage.tabulate IntGrayscaleImage.RowMajor
    ( radius*2+1, 
      radius*2+1,
      fn ( y, x ) =>
      let
        val offsetX = x-radius
        val offsetY = y-radius
      in
        if offsetY*offsetY+offsetX*offsetX <= radius*radius then fill
        else 0
      end )


  fun gradientQuantized( image : IntGrayscaleImage.image,
                         bins : int, 
                         nori : int,
                         radius : int,
                         ( savMaj, savMin ) : real * real,
                         smoothingSigma : real option )
    : RealGrayscaleImage.image list =
  let
    val ( height, width ) = IntGrayscaleImage.dimensions image
    val weights = buildCircleImage( radius, 1 )
    val sliceMap = buildOrientationSliceMap( 2*radius+1, 2*radius+1, nori )
    val nbins = ( GrayscaleMath.maxInt image )+1

    val gradients = Vector.tabulate
      ( nori, fn _ => RealGrayscaleImage.zeroImage( height, width ) )

    val smoothKernel = 
        if not ( Option.isSome smoothingSigma ) then 
          RealGrayscaleImage.fromList [ [ 0.0 ] ]
        else
          FilterUtil.createGaussianMaskgPb 0
          ( ( Option.valOf smoothingSigma )*(real nbins), 
              Real.ceil( ( Option.valOf smoothingSigma )*3.0 ) )
    
    val _ = IntGrayscaleImage.appi IntGrayscaleImage.RowMajor
      ( fn ( y, x, p ) =>
        let
          val sliceHist = Vector.tabulate
            ( 2*nori, fn _ => Array.array( nbins, 0.0 ) )

          val _ = IntGrayscaleImage.appi IntGrayscaleImage.RowMajor
            ( fn ( wy, wx, wp ) =>
              if y+wy-radius<0 orelse y+wy-radius>=height-1 orelse 
                 x+wx-radius<0 orelse x+wx-radius>=width-1 then ()
              else
              let
                val sliceHistIdx = Array2.sub( sliceMap, wy, wx )
                val sliceHist = Vector.sub( sliceHist, sliceHistIdx )

                val value = IntGrayscaleImage.sub
                  ( image, y+wy-radius, x+wx-radius )

                val oldCount = Array.sub( sliceHist, value )

                val _ = Array.update( sliceHist, value, oldCount+( real wp ) )
              in
                ()
              end )
            ( IntGrayscaleImage.full weights )

          val _ = if( x=50 andalso y=50 ) then
                  Vector.app ( 
                    fn x => (PrintUtil.printArray Real.toString x; print "\n"))                     sliceHist
                  else ()

          val _ = if Option.isSome smoothingSigma then 
                    Vector.app( fn slice => convolve( slice, smoothKernel ) )
                      sliceHist
                  else ()

          val _ = Vector.app(
            fn slice => 
            let
              val sum = Array.foldl Real.+ 0.0 slice
            in
              if not ( Real.==( sum, 0.0 ) ) then
                 Array.modify ( fn c => c/sum ) slice
              else ()
            end )
            sliceHist 

         val left = Array.array( nbins, 0.0 )
         val right = Array.array( nbins, 0.0 )
       
         val _ = Util.loopFromToInt
           ( fn orientation => 
             let
               val slice1 = Vector.sub( sliceHist, orientation )
               val slice2 = Vector.sub( sliceHist, orientation+nori )

               val _ = ArrayUtil.addReal'( left, slice1 )
               val _ = ArrayUtil.addReal'( right, slice2 )
             in
               ()
             end )
           ( 0, nori-1, 1 )


         fun printHistogram(hist) =
           Array.app( fn x => print( Real.toString x ^ ", " )) hist 

         val _ = Util.loopFromToInt
           ( fn orientation =>
             let
               val distance = MathUtil.chiSquared( left, right )
(*
               val _ = printHistogram left
               val _ = print "\n"
               val _ = printHistogram right
               val _ = print "\n"
*)
               val _ = RealGrayscaleImage.update( 
                 Vector.sub( gradients, orientation ),
                 y, 
                 x,
                 distance )

               val slice1 = Vector.sub( sliceHist, orientation )
               val slice2 = Vector.sub( sliceHist, orientation+nori )

               val _ = ArrayUtil.subtractReal'( left, slice1 )
               val _ = ArrayUtil.addReal'( left, slice2 )
               val _ = ArrayUtil.addReal'( right, slice1 )
               val _ = ArrayUtil.subtractReal'( right, slice2 )
             in 
               ()
             end )
           ( 0, nori-1, 1 )
          (*val _ = print "\n\n\n\n end\n\n"*)
        in
          ()
        end )
      ( IntGrayscaleImage.full image )
     
  in
    List.tabulate( nori, 
    fn i =>  Vector.sub( gradients, i ) )
(*        FilterUtil.savgol(
          Vector.sub( gradients, i ), 
          savMaj, 
          savMin, 
          ( (real i)*Math.pi)/(real nori)+Math.pi/2.0 ) ) *)
  end

  fun gradientReal( image : RealGrayscaleImage.image, 
                    bins : int,
                    nori : int,
                    radius : int,
                    savgol : real * real,
                    smoothingSigma : real option )
    : RealGrayscaleImage.image list =
  let
    val quantized = ImageUtil.quantizeImage( image, bins )
    val _ = print( " min: " ^ Real.toString( GrayscaleMath.minReal image ) )
    val _ = print( " max: " ^ Real.toString( GrayscaleMath.maxReal image ) )
    val _ = print "\n"
  in
    gradientQuantized( quantized, bins, nori, radius, savgol, smoothingSigma )
  end


end
