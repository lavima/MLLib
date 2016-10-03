(*
* file: test_fh.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the FH structure in the image 
* library.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FH", what="segment",
    genInput=
      fn() => [ 
        ( Option.valOf( RealPGM.read"resources/proper3.raw.pgm" ), 
          1.0, 2.0, 20.0 ),
        ( Option.valOf( RealPGM.read"resources/simple.segment.raw.pgm" ), 
          1.0, 1.0, 1.0 ) ] ,
    f= fn[ i1, i2 ] => [ FH.segment i1, FH.segment i2 ] ,
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
    inputToString= fn( im, _, _, _ ) => RealGrayscaleImage.toString im }

