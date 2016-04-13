(* 
* file: test_filter_util.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the filter utilities
*)

val _ = print"\n\n********** FilterUtil tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FilterUtil", what="createGaussianMaskgPb",
    genInput= fn() => [ ( 0, ( 3.0, 9 ) ) ] ,
    f= fn[ i1 ] => [ FilterUtil.createGaussianMaskgPb ( #1 i1 ) ( #2 i1 ) ] ,
    evaluate= fn[ o1 ] => [ true ] ,
    inputToString= 
      fn( ord, ( s, t ) ) =>
        "( " ^ 
        Int.toString ord ^
        "( " ^ Real.toString s ^ ", " ^ Int.toString t ^ " )" ^
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FilterUtil", what="createGaussianMaskGPB2D",
    genInput= 
      fn() => 
        [ ( 2, ( 3.0, 3.0, 3.0, 1.0, false, 0.3 ) ),
          ( 0, ( 3.0, 9.0, 9.0, 3.0, false, 0.3 ) ) ] ,
    f= 
      fn[ i1, i2 ] => 
        [ FilterUtil.createGaussianMaskGPB2D ( #1 i1 ) ( #2 i1 ),
          FilterUtil.createGaussianMaskGPB2D ( #1 i2 ) ( #2 i2 ) ] ,
    evaluate= 
      fn[ o1, o2 ] => 
      let
        val norm = ImageUtil.normalizeReal'' o1 
        val _ = RealPGM.write( norm, "output/output3dFilter1.pgm" )
        val norm = ImageUtil.normalizeReal'' o2 
        val _ = RealPGM.write( norm, "output/output3dFilter2.pgm" )
      in
        [ true, true ] 
      end ,
    inputToString= 
      fn( ord, ( s, sx, sy, e, h, ori ) ) =>
        "( " ^ 
        Int.toString ord ^
          "( " ^ Real.toString s ^ ", " ^ 
          Real.toString sx ^ ", " ^
          Real.toString sy ^ ", " ^
          Real.toString e ^ ", " ^
          Bool.toString h ^ ", " ^
          Real.toString ori ^ " )" ^
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FilterUtil", what="savgol",
    genInput= 
      fn() => 
        [ ( Option.valOf( RealPGM.read "proper.plain.pgm" ), 3.0, 1.0, 0.6 ) ] ,
    f= fn[ i1 ] => [ FilterUtil.savgol i1 ] ,
    evaluate= 
      fn[ o1 ] =>
      let
        val o1' = ImageUtil.normalizeReal'' o1
        val _ = RealPGM.write( o1', "output/savgolFiltering.pgm" )
      in
        [ true ] 
      end,
    inputToString=
      fn( i, ra, rb, t ) =>
        "( " ^ 
        RealGrayscaleImage.toString i ^ ", " ^
        Real.toString ra ^ ", " ^ 
        Real.toString rb ^ ", " ^ 
        Real.toString t ^ 
        " )" }


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FilterUtil", what="applyGammaCorrection",
    genInput= 
      fn() =>
        [ ( RealGrayscaleImage.fromList[ 
              [ 0.1, 0.2, 0.3 ],
              [ 0.4, 0.5, 0.6 ],
              [ 0.7, 0.8, 0.9 ] ],
            2.5 ) ] , 
    f= fn[ i1 ] => [ ( FilterUtil.applyGammaCorrection i1; #1 i1 ) ] ,
    evaluate=
      fn[ o1 ] =>
      let
        val truth = 
          RealGrayscaleImage.fromList[ 
            [ 0.003162278, 0.017888544, 0.04929503 ],
            [ 0.101192885, 0.176776695, 0.278854801 ],
            [ 0.409963413, 0.572433402, 0.768433471 ] ]
      in
        [ ImageUtil.approxCompareGrayscaleReal( o1, truth, 5 ) ]
      end ,
    inputToString=
      fn( i, s ) =>
        "( " ^ 
        RealGrayscaleImage.toString i ^ ", " ^ 
        Real.toString s ^
        " )" }
