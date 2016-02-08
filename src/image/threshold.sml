(*
* filename: thresholds.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains signatures and functor structure that provide different 
* algorithms for automatically determining threshold values in images.
*)

signature THRESHOLDS_IMAGE =
sig
  
  type pixel
  type image = { Width : int, Height : int, Values : pixel Array.array }

  val histograms : image * int -> int Array.array list

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

  fun percentage( Image : Image.image, NumBins : int, Percentage : real ) 
      : real list =
  let
    val { Width, Height, ... } = Image

    val [ Histogram ] = Image.histograms( Image, NumBins )

    val NumPixels = Width*Height

    val NormalizedHistogram = Array.array( NumBins, 0.0 ) 
    val _ = Array.appi
      ( fn( I, X ) =>
          Array.update( NormalizedHistogram, I, 
                        real X/real NumPixels ) )
      Histogram

    val Inc = 1.0/( real NumBins )

    val ( _, _, Threshold ) = 
      Array.foldl
        ( fn( X, ( Sum, Current, Threshold ) ) => 
            if Sum>Percentage andalso not( Threshold>0.0 ) then
              ( Sum+X, Current+Inc, Current )
            else
              ( Sum+X, Current+Inc, Threshold ) )
        ( 0.0, 0.0, 0.0 )
        NormalizedHistogram
  in
    [ Threshold ]
  end

  fun otsu ( Image : Image.image, NumBins : int ) : real list =
  let
    val { Width, Height, ... } = Image

    val [ Histogram ] = Image.histograms( Image, NumBins )

    val NumPixels = Width*Height

    val NormalizedHistogram = Array.array( NumBins, 0.0 ) 
    val _ = Array.appi
      ( fn( I, X ) =>
          Array.update( NormalizedHistogram, I, 
                        real X/real NumPixels ) )
      Histogram

    val Mean =
      Array.foldli
        ( fn( I, P, Mean ) =>
            Mean+( real I*P ) )
        0.0
        NormalizedHistogram

    val ( _, Thresholds ) =
      Array.foldli
        ( fn( I, _, ( Max, Thresholds ) ) => 
          let
            val ( ProbBack, CumMean ) =
              Array.foldli
                ( fn( J, P, ( PB, CM ) ) =>
                  if J<=I then
                    ( PB+P, CM+real J*P ) 
                  else
                    ( PB, CM ) )
                ( 0.0, 0.0 )
                NormalizedHistogram

            val VarClass = 
              let
                val X = Mean*ProbBack-CumMean
              in
                X*X/( ProbBack*( 1.0-ProbBack ) )
              end

          in
            if Real.isNan VarClass orelse VarClass<Max then
              ( Max, Thresholds )
            else if VarClass>Max then
              ( VarClass, [ real I ] )
            else 
              ( Max, ( real I )::Thresholds )
          end )
        ( 0.0, [ 0.0 ] )
        NormalizedHistogram
  in
    [ ( MathUtil.avg Thresholds )/real( NumBins-1 ) ]
  end

end (* structure Threshold *)
