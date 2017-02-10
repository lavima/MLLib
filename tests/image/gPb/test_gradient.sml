(* 
* file: test_gradient.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the gradient
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Gradient", what="Oriented gradient",
    genInput=
      fn() => 
        [ ( Option.valOf(RealPGM.read "resources/proper.plain.pgm" ), 
            32, 
            8, 
            16, 
            ( 5.0, 5.0/4.0 ), 
            SOME(0.1) ) ] ,
    f= fn[ i1 ] => [ GradientSquare.gradientReal i1 ] ,
    evaluate= 
      fn[ o1::rest ] => 
      let
        val normalizedImage = ImageUtil.normalizeReal'' o1
        val _ = RealPGM.write(normalizedImage, "output/Orientedgradient.pgm")
      in
        [ true ]
      end ,
    inputToString=
      fn( i, b, n, r, ( x1, x2 ), y ) =>
        "( " ^ 
        RealGrayscaleImage.toString i ^ ", " ^ 
        Int.toString b ^ ", " ^
        Int.toString r ^ ", " ^
        Int.toString n ^ ", " ^ 
        "( " ^ Real.toString x1 ^ ", " ^ Real.toString x2 ^ " )," ^ 
        Real.toString( Option.valOf y ) ^ 
        " )" }
