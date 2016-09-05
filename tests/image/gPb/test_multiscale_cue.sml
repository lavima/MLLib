(* 
* file: test_texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests for validating texton generation
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="MultiscaleCue", what="Generate multiscale cue response",
    genInput=
      fn() =>
        [ ( { channelL = {
              weights = [ 0.0146, 0.0145, 0.0163 ],
              savgolFilters = 
                [ ( 3.0, 3.0/4.0), ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ) ],
              histogramSmoothSigma = SOME( 0.1 ),
              bins = 25,
              scale = [ 3, 5, 10 ],
              nori = 8
            },
            channelA = {
              weights = [ 0.0210, 0.0243, 0.0287 ],
              savgolFilters = 
                [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
              histogramSmoothSigma = SOME( 0.05 ),
              bins = 25,
              scale = [ 5, 10, 20 ],
              nori = 8
            },
            channelB = {
              weights = [ 0.0166, 0.0185, 0.0204 ],
              savgolFilters = 
                [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
              histogramSmoothSigma = SOME( 0.05 ),
              bins = 25,
              scale = [ 5, 10, 20 ],
              nori = 8
            },
            channelT = {
              weights = [ 0.0101, 0.0111, 0.0141 ],
              savgolFilters = 
                [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
              histogramSmoothSigma = NONE,
              bins = 32,
              scale = [ 5, 10, 20 ],
              nori = 8
            },
            texton = {
              nori = 8,
              sigma = [ 2.0, 2.0 * ( Math.sqrt 2.0 ) ],
              nTextons = 64,
              maxIterations = 5
            },
            border = 30,
            gradientQuantized = GradientDisk.gradientQuantized,
            gradientReal = GradientDisk.gradientReal }, 
          Option.valOf( RealPPM.read "proper2.raw.ppm" ) ) ] ,
    f= fn[ i1 ] => [ MultiscaleCue.multiscale ( #1 i1 ) ( #2 i1 ) ] ,
    evaluate=
      fn[ result ] =>
      let
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
        val map = buildOrientationSliceMap(11, 11, 8)
        val mapStr = Array2Util.toString Int.toString map
        val _ = print( mapStr ^ "\n" )

        fun writeAllOrientations(filename, [], a) = ()
          | writeAllOrientations(filename, b::rest, a) =
        let
          val (height, width) = RealGrayscaleImage.dimensions b
          val _ = TextFileUtil.writeDSV
                    ( ",", width ) 
                    ( Real.toString  )
                    ( Array2Util.toFlatList b , 
                      "output/" ^ filename ^ ( Int.toString a ) ^ ".csv" )
        in
          writeAllOrientations(filename, rest, a+1)
        end

        fun writeAllScales(filename, [], a) = ()
          | writeAllScales(filename, b::rest, a) =
        let
         val _ = writeAllOrientations(filename ^ ( Int.toString a ) ^ "_", b, 0)
        in
          writeAllScales(filename, rest, a+1)
        end


        val _ = writeAllOrientations( "MultiCombined", #combined result, 0 )
        val _ = writeAllScales( "ChannelL", #channelL result, 0 )
        val _ = writeAllScales( "channelA", #channelA result, 0 )
        val _ = writeAllScales( "channelB", #channelB result, 0 )
        val _ = writeAllScales( "Texton", #texton result, 0 )

      in
        [ true ]
      end ,
    inputToString = fn( c ) => "" (* RealRGBImage.toString i *) }

