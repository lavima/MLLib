(* 
* file: test_filter_util.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the image utility functionality.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ImageUtil", what="Approximate real grayscale image comparison",
    genInput= 
      fn() => 
      let
        val im1 =   
          RealGrayscaleImage.fromList[ 
            [ 0.11, 0.21, 0.35 ],
            [ 0.42, 0.53, 0.62 ],
            [ 0.73, 0.85, 0.91 ] ]
        val im2 = 
          RealGrayscaleImage.fromList[ 
            [ 0.10, 0.21, 0.35 ],
            [ 0.41, 0.53, 0.62 ],
            [ 0.73, 0.82, 0.91 ] ]
      in
        [ ( im1, im1, 2 ), ( im1, im2, 2 ) ]
      end ,
    f= 
      fn[ i1, i2 ] => 
        [ ImageUtil.approxCompareGrayscaleReal i1,
          ImageUtil.approxCompareGrayscaleReal i2 ] ,
    evaluate= fn[ o1, o2 ] => [ o1, not o2 ] ,
    inputToString= 
      fn( i1, i2, n ) =>
        "( " ^ 
        RealGrayscaleImage.toString i1 ^ ", " ^ 
        RealGrayscaleImage.toString i2 ^ ", " ^
        Int.toString n ^
        " )" }

local
  val cieImageData = 
    [ [ ( 0.11, 0.21, 0.71 ), ( 0.21, 0.61, 0.41 ), ( 0.35, 0.95, 0.65 ) ],
      [ ( 0.42, 0.52, 0.52 ), ( 0.53, 0.73, 0.23 ), ( 0.62, 0.72, 0.52 ) ],
      [ ( 0.73, 0.43, 0.33 ), ( 0.85, 0.15, 0.05 ), ( 0.91, 0.41, 0.31 ) ] ]
in

  val _ = 
    SimpleTest.test' ( CommandLine.arguments() ) {
      group="ImageUtil", what="get A channel from real CIE lab",
      genInput= fn() => [ RealCIELabImage.fromList cieImageData ] ,
      f= fn[ i1 ] => [ ImageUtil.getAChannel i1 ] ,
      evaluate=
        fn[ o1 ] =>
        let
          val expected = 
            RealGrayscaleImage.fromList[ 
              [ 0.21, 0.61, 0.95 ],
              [ 0.52, 0.73, 0.72 ],
              [ 0.43, 0.15, 0.41 ] ]
        in
          [ RealGrayscaleImage.equal( o1, expected ) ]
        end ,
      inputToString = RealCIELabImage.toString }

  val _ = 
    SimpleTest.test' ( CommandLine.arguments() ) {
      group="ImageUtil", what="get B channel from real CIE lab",
      genInput= fn() => [ RealCIELabImage.fromList cieImageData ] ,
      f= fn[ i1 ] => [ ImageUtil.getBChannel i1 ] ,
      evaluate= 
        fn[ o1 ] =>
        let
          val expected = 
            RealGrayscaleImage.fromList[ 
              [ 0.71, 0.41, 0.65 ],
              [ 0.52, 0.23, 0.52 ],
              [ 0.33, 0.05, 0.31 ] ]
        in
          [ RealGrayscaleImage.equal( o1, expected ) ]
        end ,
      inputToString = RealCIELabImage.toString }

  val _ = 
    SimpleTest.test' ( CommandLine.arguments() ) {
      group="ImageUtil", what="get L channel from real CIE lab",
      genInput= fn() => [ RealCIELabImage.fromList cieImageData ] ,
      f= fn[ i1 ] => [ ImageUtil.getLChannel i1 ] ,
      evaluate= 
        fn[ o1 ] =>
        let
          val expected = 
            RealGrayscaleImage.fromList[ 
              [ 0.11, 0.21, 0.35 ],
              [ 0.42, 0.53, 0.62 ],
              [ 0.73, 0.85, 0.91 ] ]
        in
          [ RealGrayscaleImage.equal( o1, expected ) ]
        end ,
      inputToString= RealCIELabImage.toString }

end
