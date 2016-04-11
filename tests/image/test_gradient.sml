(* 
* file: test_gradient.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the gradient
*)

val _ = print"\n\n********** Gradient tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Gradient", what="gradient", 
    genInput=
      fn() => [ ( Option.valOf(RealPGM.read "proper.plain.pgm" ), 32, 7 ) ] ,
    f= fn[ i1 ] => [ Gradient.gradient i1 ] ,
    evaluate = fn[ o1 ] => [ true ] ,
    inputToString=
      fn( i, b, r ) =>
        "( " ^ 
        RealGrayscaleImage.toString i ^ ", " ^ 
        Int.toString b ^ ", " ^
        Int.toString r ^
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Gradient", what="Oriented gradient",
    genInput=
      fn() => 
        [ ( Option.valOf(RealPGM.read "proper.plain.pgm" ), 32, 20, 0.6 ) ] ,
    f= fn[ i1 ] => [ Gradient.orientedGradient i1 ] ,
    evaluate= fn[ o1 ] => [ true ] ,
    inputToString=
      fn( i, b, r, ori ) =>
        "( " ^ 
        RealGrayscaleImage.toString i ^ ", " ^ 
        Int.toString b ^ ", " ^
        Int.toString r ^ ", " ^
        Real.toString ori ^
        " )" }
