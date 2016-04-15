(* 
* file: test_texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests for validating texton generation
*)

val _ = print"\n\n********** Multiscale cue tests **********\n"


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
              scale = 5,
              nori = 8
            },
            channelA = {
              weights = [ 0.0210, 0.0243, 0.0287 ],
              savgolFilters = 
                [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
              histogramSmoothSigma = SOME( 0.05 ),
              bins = 25,
              scale = 10,
              nori = 8
            },
            channelB = {
              weights = [ 0.0166, 0.0185, 0.0204 ],
              savgolFilters = 
                [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
              histogramSmoothSigma = SOME( 0.05 ),
              bins = 25,
              scale = 10,
              nori = 8
            },
            channelT = {
              weights = [ 0.0101, 0.0111, 0.0141 ],
              savgolFilters = 
                [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
              histogramSmoothSigma = NONE,
              bins = 32,
              scale = 10,
              nori = 8
            },
            texton = {
              nori = 8,
              sigma = [ 2.0, 2.0 * ( Math.sqrt 2.0 ) ],
              nTextons = 32,
              maxIterations = 200
            },
            border = 30,
            gradientQuantized = GradientDisk.gradientQuantized,
            gradientReal = GradientDisk.gradientReal }, 
          Option.valOf( RealPPM.read "proper2.raw.ppm" ) ) ] ,
    f= fn[ i1 ] => [ MultiscaleCue.multiscale ( #1 i1 ) ( #2 i1 ) ] ,
    evaluate=
      fn[ result ] =>
      let
        val comb = #combined result
        val [ c1, c2, c3, c4, c5, c6, c7, c8 ] = comb
        val _ = RealPGM.write(ImageUtil.normalizeReal'' c1, "output/MutC1.pgm" )
        val _ = RealPGM.write(ImageUtil.normalizeReal'' c2, "output/MutC2.pgm" )
        val _ = RealPGM.write(ImageUtil.normalizeReal'' c3, "output/MutC3.pgm" )
        val _ = RealPGM.write(ImageUtil.normalizeReal'' c4, "output/MutC4.pgm" )
        val _ = RealPGM.write(ImageUtil.normalizeReal'' c5, "output/MutC5.pgm" )
        val _ = RealPGM.write(ImageUtil.normalizeReal'' c6, "output/MutC6.pgm" )
        val _ = RealPGM.write(ImageUtil.normalizeReal'' c7, "output/MutC7.pgm" )
        val _ = RealPGM.write(ImageUtil.normalizeReal'' c8, "output/MutC8.pgm" )
      in
        [ true ]
      end ,
    inputToString = fn( c ) => "" (* RealRGBImage.toString i *) }
