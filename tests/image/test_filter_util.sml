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
    genInput= fn() => [ ( 2, ( 3.0, 3.0, 3.0, 1.0, false, 0.3 ) ) ] ,
    f= fn[ i1 ] => [ FilterUtil.createGaussianMaskGPB2D ( #1 i1 ) ( #2 i1 ) ] ,
    evaluate= fn[ o1 ] => [ true ] ,
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
    evaluate= fn[ o1 ] => [ true ] ,
    inputToString=
      fn( i, ra, rb, t ) =>
        "( " ^ 
        RealGrayscaleImage.toString i ^ ", " ^
        Real.toString ra ^ ", " ^ 
        Real.toString rb ^ ", " ^ 
        Real.toString t ^ 
        " )" }

