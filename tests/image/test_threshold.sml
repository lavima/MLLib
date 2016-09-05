(* 
* file: test_image_threshold.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the Threshold structure in the image 
* library.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealGrayscaleThreshold", what="percentage",
    genInput= 
      fn() => [ ( Option.valOf( RealPGM.read("simple.plain.pgm") ), 4, 0.5 ) ] ,
    f= fn[ i1 ] => [ RealGrayscaleThreshold.percentage i1 ] ,
    evaluate= fn[ o1 ] => [ Real.==( o1, 0.75 ) ] ,
    inputToString=
      fn( i, n, p ) =>
        "( " ^
        RealGrayscaleImage.toString i ^ ", " ^
        Int.toString n ^ ", " ^
        Real.toString p ^
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealGrayscaleThreshold", what= "otsu",
    genInput=
      fn() =>
        [ ( Option.valOf( RealPGM.read("simple.plain.pgm") ), 256 ),
          ( Option.valOf( RealPGM.read("proper.raw.pgm") ), 256 ) ] ,
    f= 
      fn[ i1, i2 ] => 
        [ RealGrayscaleThreshold.otsu i1, RealGrayscaleThreshold.otsu i2 ] ,
    evaluate =
      fn[ o1, o2 ] => 
        [ Util.approxEqReal( o1, 0.749019607843137, 3 ),
          Util.approxEqReal( o2, 0.439215686274510, 3 ) ] ,
    inputToString=
      fn( i, n ) =>
        "( " ^
        RealGrayscaleImage.toString i ^ ", " ^
        Int.toString n ^ 
        " )" }

