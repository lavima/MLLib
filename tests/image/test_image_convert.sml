(* 
* file: test_filter_convert.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate image conversion functionality.
*)


val _ = print"\n\n********** Image conversion tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ImageConvert", what="Convert real RGB image to grayscale",
    genInput= 
      fn() =>
        [ RealRGBImage.fromList[ 
            [ ( 0.11, 0.32, 0.23 ), ( 0.21, 0.13, 0.42 ) ],
            [ ( 0.42, 0.31, 0.23 ), ( 0.53, 0.65, 0.38 ) ] ] ] ,
    f= fn[ i1 ] => [ ImageConvert.realRGBtoGray i1 ] ,
    evaluate= 
      fn[ o1 ] =>
      let
        val expected = 
          RealGrayscaleImage.fromList[ 
            [ 0.2469608 , 0.186981 ],
            [ 0.3337618 , 0.583342 ] ]
      in
        [ ImageUtil.approxCompareGrayscaleReal ( expected, o1, 4 ) ]
      end ,
    inputToString= RealRGBImage.toString }
