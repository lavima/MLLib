(*
* file: gradient_disk.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for calculating gradients using 
* circular disks.
* 
*)

structure GradientDisk : GRADIENT =
struct

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
    Array.modifyi (fn (i, p) => RealGrayscaleImage.sub(convolved, 0, i) ) data
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
        if offsetY*offsetY+offsetX*offsetX < radius*radius then fill
        else 0
      end )

  fun buildOrientationSliceMap( height : int, width : int, nori : int )
    : int Array2.array =
  let
    val sliceMap = Array2.array( height, width, 0 )

    val _ = Array2.modifyi Array2.RowMajor 
    ( fn ( y, x, _ ) => 
      let
        val ori = Math.atan2
          ( ( real y )-( real height )/2.0, 
            ( real x )-( real width )/2.0 )+Math.pi
        val index = Real.floor( ori/Math.pi*( real nori ) )
      in
        if index >= 2*nori then 2*nori-1
        else index
      end )
    { base=sliceMap, row=0, col=0, nrows=NONE, ncols=NONE }
  in
    sliceMap
  end

  fun gradientQuantized( image : IntGrayscaleImage.image,
                         bins : int, 
                         nori : int,
                         radius : int,
                         savgol : real * real,
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

          val _ = Vector.app(
            fn slice => 
            let
              val sum = Array.foldl Real.+ 0.0 slice
            in
              if sum > 0.0 then
                Array.modify ( fn c => c / sum ) slice
              else ()
            end )
            sliceHist

          val _ = if Option.isSome smoothingSigma then 
                    Vector.app( fn slice => convolve( slice, smoothKernel ) )
                      sliceHist
                  else ()

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
             end
            )
           ( 0, nori-1, 1 )


         val _ = Util.loopFromToInt
           ( fn orientation =>
             let
               val _ = RealGrayscaleImage.update( 
                 Vector.sub( gradients, orientation ),
                 y, 
                 x,
                 MathUtil.chiSquared( left, right ) )

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

        in
          ()
        end )
      ( IntGrayscaleImage.full image )
     
  in
    List.tabulate(nori, fn i => Vector.sub( gradients, i ) )
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
  in
    gradientQuantized( quantized, bins, nori, radius, savgol, smoothingSigma )
  end


end
