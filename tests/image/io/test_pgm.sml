(* 
* file: test_pgm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the PGM implementations.
*)

val _ = print"\n\n********** PGM Tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="read",
    genInput= fn() => [ "simple.plain.pgm", "simple.raw.pgm" ] ,
    f= 
      fn[ i1, i2 ] => [ 
        Option.valOf( Word8PGM.read i1 ), 
        Option.valOf( Word8PGM.read i2 ) ] ,
    evaluate= 
      fn[ o1, o2 ] => 
      let
        val truth = 
          Word8GrayscaleImage.fromList[ [ 0w0, 0w64 ], [ 0w128, 0w255 ] ]
      in [ 
        Word8GrayscaleImage.equal( o1, truth ),
        Word8GrayscaleImage.equal( o2, truth ) ]
      end ,
    inputToString= fn x => x }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="write",
    genInput= 
      fn() => [ 
        ( [ [ 0w0, 0w64 ], [ 0w128, 0w255 ] ], "output/write.plain.pgm" ), 
        ( [ [ 0w0, 0w64 ], [ 0w128, 0w255 ] ], "output/write.raw.pgm" ) ] , 
    f=
      fn[ i1, i2 ] =>
      let
        val im1 = Word8GrayscaleImage.fromList ( #1 i1 )
        val im2 = Word8GrayscaleImage.fromList ( #1 i2 )

        val _ = Word8PGM.write( im1, #2 i1 )
        val _ = Word8PGM.write' ( PNM.rawPGM, 0w255 ) ( im2, #2 i2 )
      in
        [ #2 i1, #2 i2 ]
      end ,
    evaluate=
      fn[ o1, o2 ] => 
      let
        val im1 = Option.valOf( Word8PGM.read o1 )
        val im2 = Option.valOf( Word8PGM.read o2 )

        val truth = 
          Word8GrayscaleImage.fromList[ [ 0w0, 0w64 ], [ 0w128, 0w255 ] ]
      in [ 
        Word8GrayscaleImage.equal( im1, truth ) andalso
        Word8GrayscaleImage.equal( im2, truth ) ]
      end ,
    inputToString= 
      fn( xss, f )=> 
        "( " ^ 
        ListUtil.toString ListUtil.toString Word8.toString xss ^
        ", " ^ 
        f ^ 
        " )" } 

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealPGM", what="read",
    genInput= 
      fn() => [ 
        "simple.plain.pgm", 
        "simple.raw.pgm", 
        "proper.plain.pgm",
        "proper.raw.pgm" ] ,
    f= 
      fn[ i1, i2, i3, i4 ] => [ 
        Option.valOf( RealPGM.read i1 ), 
        Option.valOf( RealPGM.read i2 ),
        Option.valOf( RealPGM.read i3 ),
        Option.valOf( RealPGM.read i4 ), ] ,
    evaluate= 
      fn[ o1, o2, o3, o4 ] => 
      let
        val simpleTruth = 
          RealGrayscaleImage.fromList[ 
            [ 0.0, 64.0/255.0 ], 
            [ 128.0/255.0, 1.0 ] ]
        val properTruth = 
          Array.fromList( TextFileUtil.readCSReals "proper.real.csv" )

        fun equalsProperTruth im = 
          RealGrayscaleImage.foldi RealGrayscaleImage.RowMajor
            ( fn( i, j, x, equal ) =>
                equal andalso 
                Util.approxEqReal( x, Array.sub( properTruth, i*width+j ), 3 ) )
            true
            ( RealGrayscaleImage.full im ),       
      in [ 
        RealGrayscaleImage.equal( o1, simpleTruth ),
        RealGrayscaleImage.equal( o2, simpleTruth ),
        equalsProperTruth o3, 
        equalsProperTruth o4 ]
      end ,
    inputToString= fn x => x }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealPGM", what="write",
    genInput= 
      fn() => [ 
        ( [ [ 0.0, 0.64/255.0 ], [ 0.128/255.0, 1.0 ] ], 
          "output/write.plain.pgm" ), 
        ( [ [ 0.0, 0.64/255.0 ], [ 0.128/255.0, 1.0 ] ], 
          "output/write.raw.pgm" ) ] , 
    f=
      fn[ i1, i2 ] =>
      let
        val im1 = RealGrayscaleImage.fromList ( #1 i1 )
        val im2 = RealGrayscaleImage.fromList ( #1 i2 )

        val _ = RealPGM.write( im1, #2 i1 )
        val _ = RealPGM.write' ( PNM.rawPGM, 0w255 ) ( im2, #2 i2 )
      in
        [ #2 i1, #2 i2 ]
      end ,
    evaluate=
      fn[ o1, o2 ] => 
      let
        val im1 = Option.valOf( Word8PGM.read o1 )
        val im2 = Option.valOf( Word8PGM.read o2 )

        val truth = 
          Word8GrayscaleImage.fromList[ [ 0w0, 0w64 ], [ 0w128, 0w255 ] ]
      in [ 
        Word8GrayscaleImage.equal( im1, truth ) andalso
        Word8GrayscaleImage.equal( im2, truth ) ]
      end ,
    inputToString= fn x => x }

