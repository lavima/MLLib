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
      fn() => 
        [ ( Option.valOf(RealPGM.read "proper.plain.pgm" ), 32, 7, 0.1 ) ] ,
    f= fn[ i1 ] => [ Gradient.gradient i1 ] ,
    evaluate = 
      fn[ o1 ] => 
      let
        val normalizedImage = ImageUtil.normalizeReal'' o1
        val _ = RealPGM.write(normalizedImage, "output/gradient.pgm")
      in 
        [ true ] 
      end ,
    inputToString=
      fn( i, b, r, x ) =>
        "( " ^ 
        RealGrayscaleImage.toString i ^ ", " ^ 
        Int.toString b ^ ", " ^
        Int.toString r ^ ", " ^ 
        Real.toString x ^
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Gradient", what="Oriented gradient",
    genInput=
      fn() => 
        [ ( Option.valOf(RealPGM.read "proper.plain.pgm" ), 
            32, 
            20, 
            0.6, 
            ( 5.0, 5.0/4.0 ), 
            0.1 ) ] ,
    f= fn[ i1 ] => [ Gradient.orientedGradientReal i1 ] ,
    evaluate= 
      fn[ o1 ] => 
      let
        val normalizedImage = ImageUtil.normalizeReal'' o1
        val _ = RealPGM.write(normalizedImage, "output/Orientedgradient.pgm")
      in
        [ true ]
      end ,
    inputToString=
      fn( i, b, r, ori, ( x1, x2 ), y ) =>
        "( " ^ 
        RealGrayscaleImage.toString i ^ ", " ^ 
        Int.toString b ^ ", " ^
        Int.toString r ^ ", " ^
        Real.toString ori ^ ", " ^ 
        "( " ^ Real.toString x1 ^ ", " ^ Real.toString x2 ^ " )," ^ 
        Real.toString y ^ 
        " )" }
