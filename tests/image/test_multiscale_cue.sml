(* 
* file: test_texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests for validating texton generation
*)

val _ = print"\n\n********** Multiscale cue tests **********\n"


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="MultiscaleCue", what="orientedMultiscale",
    genInput=
      fn() =>
      let
        val im = Option.valOf( RealPGM.read "proper.plain.pgm" )
        val ( height, width ) = RealGrayscaleImage.dimensions im
      in
        [ ( height, 
            width,
            im,
            Gradient.orientedGradientReal,
            [ 1.0, 1.0, 1.0 ],
            32,
            10,
            Math.pi/4.0, 
            [ ( 3.0, 3.0/4.0 ), ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ) ], 
            0.1 ) ] 
      end ,
    f= fn[ i1 ] => [ MultiscaleCue.orientedMultiscale i1 ] ,
    evaluate= 
      fn[ o1 ] => 
      let
        val o1' = ImageUtil.normalizeReal'' o1
        val _ = RealPGM.write( o1', "output/OrientedMultiscale.pgm" )
      in
        [ true ] 
      end ,
    inputToString=
      fn( _, _, im, _, ws, b, s, ori, savgol, _ ) =>
        "( " ^ 
        RealGrayscaleImage.toString im ^ ", " ^
        ListUtil.toString Real.toString ws ^ ", " ^ 
        Int.toString b ^ ", " ^
        Int.toString s ^ ", " ^
        Real.toString ori ^ ", " ^
        ListUtil.toString 
          ( fn( x, y ) => 
              "( " ^ Real.toString x ^ ", " ^ Real.toString y ^ " )" )
          savgol ^ 
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="MultiscaleCue", what="Generate multiscale cue response",
    genInput=
      fn() =>
        [ ( { channelL = {
              weights = [ 0.0146, 0.0145, 0.0163 ],
              savgolFilters = 
                [ ( 3.0, 3.0/4.0), ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ) ],
              histogramSmoothSigma = 0.1,
              bins = 25,
              scale = 5,
              nori = 8
            },
            channelA = {
              weights = [ 0.0210, 0.0243, 0.0287 ],
              savgolFilters = 
                [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
              histogramSmoothSigma = 0.05,
              bins = 25,
              scale = 10,
              nori = 8
            },
            channelB = {
              weights = [ 0.0166, 0.0185, 0.0204 ],
              savgolFilters = 
                [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
              histogramSmoothSigma = 0.05,
              bins = 25,
              scale = 10,
              nori = 8
            },
            channelT = {
              weights = [ 0.0101, 0.0111, 0.0141 ],
              savgolFilters = 
                [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
              histogramSmoothSigma = 0.0,
              bins = 32,
              scale = 10,
              nori = 8
            },
            texton = {
              nori = 8,
              sigma = [ 2.0, 2.0 * ( Math.sqrt 2.0 ) ],
              nTextons = 32,
              maxIterations = 500
            },
            border = 30 },
          Option.valOf( RealPPM.read "proper2.raw.ppm" ) ) ] ,
    f= fn[ i1 ] => [ MultiscaleCue.multiscale ( #1 i1 ) ( #2 i1 ) ] ,
    evaluate=
      fn[ o1 ] =>
      let
        val normalizedImage = ImageUtil.normalizeReal'' o1
        val _ = RealPGM.write( normalizedImage, "output/Multiscale.pgm" )
      in
        [ true ]
      end ,
    inputToString= fn( c, i ) => RealCIELabImage.toString i }
