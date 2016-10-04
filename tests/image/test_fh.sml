(*
* file: test_fh.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the FH structure in the image 
* library.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealGrayscaleFH", what="segment",
    genInput=
      fn() => [ 
        ( ( 1.0, 2.0, 20 ),
          Option.valOf( RealPGM.read"resources/proper3.raw.pgm" ) ), 
        ( ( 1.0, 1.0, 1 ), 
          Option.valOf( RealPGM.read"resources/simple.segment.raw.pgm" ) ) ] ,
    f= 
      fn[ i1, i2 ] => [ 
        RealGrayscaleFH.segment ( #1 i1 ) ( #2 i1 ), 
        RealGrayscaleFH.segment ( #1 i2 ) ( #2 i2 ) ] ,
    evaluate= 
      fn[ o1, o2 ] =>
      let
        val ( height, width ) = IntGrayscaleImage.dimensions o1
        val o1' = IntGrayscaleImage.image( height, width, 0 )
        val _ =
          IntGrayscaleImage.modifyi IntGrayscaleImage.RowMajor
            ( fn( y, x, _ ) => IntGrayscaleImage.sub( o1, y, x ) mod 65536 )
            ( IntGrayscaleImage.full o1' )
        val t1 = Option.valOf( IntPGM.read"resources/proper3.fh.truth.pgm" )
        val t2 = Option.valOf( IntPGM.read"resources/simple.segment.fh.truth.pgm" )
      in
        [ IntGrayscaleImage.equal( o1', t1 ), IntGrayscaleImage.equal( o2, t2 ) ]
      end ,
    inputToString= fn( ( _, _, _ ), im ) => RealGrayscaleImage.toString im }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealRGBFH", what="segment",
    genInput=
      fn() => [ 
        ( ( 1.0, 2.0, 20 ),
          Option.valOf( RealPPM.read"resources/proper3.raw.ppm" ) ) ] ,
    f= 
      fn[ i1 ] => [ 
        RealRGBFH.segment ( #1 i1 ) ( #2 i1 ) ] ,
    evaluate= 
      fn[ o1 ] =>
      let
        val ( height, width ) = IntGrayscaleImage.dimensions o1
        val o1' = IntGrayscaleImage.image( height, width, 0 )
        val _ =
          IntGrayscaleImage.modifyi IntGrayscaleImage.RowMajor
            ( fn( y, x, _ ) => IntGrayscaleImage.sub( o1, y, x ) mod 65536 )
            ( IntGrayscaleImage.full o1' )
        val t1 = Option.valOf( IntPGM.read"resources/proper3.rgb_fh.truth.pgm" )
      in
        [ IntGrayscaleImage.equal( o1', t1 ) ]
      end ,
    inputToString= fn( ( _, _, _ ), im ) => RealRGBImage.toString im }

