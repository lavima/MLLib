(* 
* file: test_grayscale_math.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validatet validate basic grayscale math 
* operations
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="GrayscaleMath", what="Maximum real value",
    genInput= 
      fn() =>
        [ RealGrayscaleImage.fromList[ 
            [ 3.2, 4.3, 1.2 ],
            [ ~3.2, 5.7, 4.7 ],
            [  3.5, 2.3, 5.2 ] ] ] ,
    f= fn[ i1 ] => [ GrayscaleMath.maxReal i1 ] ,
    evaluate= fn[ o1 ] => [ Real.==( o1, 5.7 ) ] ,
    inputToString = RealGrayscaleImage.toString }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="GrayscaleMath", what="Minimum real value",
    genInput= 
      fn() =>
        [ RealGrayscaleImage.fromList[ 
            [ 3.2, 4.3, 1.2 ],
            [ ~3.2, 5.7, 4.7 ],
            [  3.5, 2.3, 5.2 ] ] ] ,
    f= fn[ i1 ] => [ GrayscaleMath.minReal i1 ] ,
    evaluate=  fn[ o1 ] => [ Real.==( o1, ~3.2 ) ] ,
    inputToString = RealGrayscaleImage.toString }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="GrayscaleMath", what="Sum real value",
    genInput= 
      fn() =>
        [ RealGrayscaleImage.fromList[ 
            [ 3.2, 4.3, 1.2 ],
            [ ~3.2, 5.7, 4.7 ],
            [  3.5, 2.3, 5.2 ] ] ] ,
    f= fn[ i1 ] => [ GrayscaleMath.sumReal i1 ] ,
    evaluate=  fn[ o1 ] => [ Real.==( o1, 26.9 ) ] ,
    inputToString = RealGrayscaleImage.toString }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="GrayscaleMath", what="Mean real value",
    genInput= 
      fn() =>
        [ RealGrayscaleImage.fromList[ 
            [ 3.2, 4.3, 1.2 ],
            [ ~3.2, 5.7, 4.7 ],
            [  3.5, 2.3, 5.2 ] ] ] ,
    f= fn[ i1 ] => [ GrayscaleMath.meanReal i1 ] ,
    evaluate=  fn[ o1 ] => [ Util.approxEqReal( o1, 2.9889, 3 ) ] ,
    inputToString = RealGrayscaleImage.toString }
