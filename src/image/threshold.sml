(*
* filename: thresholds.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains signatures and functor structure that provide different 
* algorithms for automatically determining threshold values in images.
*)

signature THRESHOLDS_IMAGE =
sig
  
  type image = { width : int, height : int, values : 'a Array.array }

  val histogram : int -> image -> int Array.array list

end

signature THRESHOLDS =
sig
  
  structure Image : THRESHOLDS_IMAGE

  val percentage : Image.image * int * real -> real list
  val otsu : Image.image * int -> real list

end

functor ThresholdsFun( Image : THRESHOLDS_IMAGE ) : THRESHOLDS =
struct

  structure Image = Image

  fun percentage( im : Image.image, numBins : int, percentage : real ) 
      : real list =
  let
    val { width, height, ... } = im

    val [ histogram ] = Image.histogram numBins im, numBins

    val numPixels = width*height

    val normalizedHistogram = Array.array( numBins, 0.0 ) 
    val _ = Array.appi
      ( fn( i, x ) =>
          Array.update( normalizedHistogram, i, 
                        real x/real numPixels ) )
      histogram

    val inc = 1.0/( real numBins )

    val ( _, _, threshold ) = 
      Array.foldl
        ( fn( x, ( sum, current, threshold ) ) => 
            if sum>percentage andalso not( threshold>0.0 ) then
              ( sum+x, current+inc, current )
            else
              ( sum+x, current+inc, threshold ) )
        ( 0.0, 0.0, 0.0 )
        normalizedHistogram
  in
    [ threshold ]
  end

  fun otsu ( im : Image.image, numBins : int ) : real list =
  let
    val { width, height, ... } = im

    val [ histogram ] = Image.histogram numBins im numBins

    val numPixels = width*height

    val normalizedHistogram = Array.array( numBins, 0.0 ) 
    val _ = Array.appi
      ( fn( i, x ) =>
          Array.update( normalizedHistogram, i, 
                        real x/real numPixels ) )
      histogram

    val mean =
      Array.foldli
        ( fn( i, p, mean ) =>
            mean+( real i*p ) )
        0.0
        normalizedHistogram

    val ( _, thresholds ) =
      Array.foldli
        ( fn( i, _, ( max, thresholds ) ) => 
          let
            val ( probBack, cumMean ) =
              Array.foldli
                ( fn( j, p, ( pb, cm ) ) =>
                  if j<=i then
                    ( pb+p, cm+real j*p ) 
                  else
                    ( pb, cm ) )
                ( 0.0, 0.0 )
                normalizedHistogram

            val varClass = 
              let
                val x = mean*probBack-cumMean
              in
                x*x/( probBack*( 1.0-probBack ) )
              end

          in
            if Real.isNan varClass orelse varClass<max then
              ( max, thresholds )
            else if varClass>max then
              ( varClass, [ real i ] )
            else 
              ( max, ( real i )::thresholds )
          end )
        ( 0.0, [ 0.0 ] )
        normalizedHistogram
  in
    [ ( MathUtil.avg thresholds )/real( numBins-1 ) ]
  end

end (* structure Threshold *)
