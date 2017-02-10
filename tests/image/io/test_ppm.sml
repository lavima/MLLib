(* 
* file: test_ppm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the PPM implementation.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealPPM", what="read",
    genInput= fn() => [ "resources/proper2.raw.ppm", "resources/proper2.plain.ppm" ] ,
    f= 
      fn[ i1, i2 ] => 
        [ Option.valOf( RealPPM.read i1 ), Option.valOf( RealPPM.read i2 ) ] ,
    evaluate=
      fn[ o1, o2 ] =>
      let
        val ( height, width ) = RealRGBImage.dimensions o1
        val truth1 = 
          Array.fromList( TextFileUtil.readCSReals "resources/proper2.real_rgb.csv" )

        fun equal( truth : real Array.array, im : RealRGBImage.image ) : bool = 
          RealRGBImage.foldi RealRGBImage.RowMajor
            ( fn( i, j, ( r, g, b ), equal ) =>
              let
                val tr = Array.sub( truth, i*(width*3)+j*3 )
                val tg = Array.sub( truth, i*(width*3)+j*3+1 )
                val tb = Array.sub( truth, i*(width*3)+j*3+2 )
              in
                if equal andalso 
                   Util.approxEqReal( r, tr , 3 ) andalso
                   Util.approxEqReal( g, tg, 3 ) andalso
                   Util.approxEqReal( b, tb, 3 ) then
                  equal
                else
                  false
              end )
            true
            ( RealRGBImage.full im )       
      in
        [ equal( truth1, o1 ), equal( truth1, o2 ) ]
      end ,
    inputToString= fn s => s } 

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealPPM", what="write",
    genInput=
      fn() =>
      let
        val im = Option.valOf( RealPPM.read "resources/proper2.raw.ppm" )
      in
        [ ( ( PNM.plainPPM, 0w255 ), ( im, "output/output.plain.ppm" ) ),
          ( ( PNM.rawPPM, 0w255 ), ( im, "output/output.raw.ppm" ) ) ]
      end ,
    f=
      fn[ i1, i2 ] =>
      let
        val _ = RealPPM.write' ( #1 i1 ) ( #2 i1 )
        val _ = RealPPM.write' ( #1 i2 ) ( #2 i2 )
      in
        [ #2( #2 i1 ), #2( #2 i2 ) ]
      end ,
    evaluate=
      fn[ o1, o2 ] =>
      let
        val out1 = Option.valOf( RealPPM.read "output/output.plain.ppm" )
        val out2 = Option.valOf( RealPPM.read "output/output.raw.ppm" )
        val ( height, width ) = RealRGBImage.dimensions out1
        val truth1 = 
          Array.fromList( TextFileUtil.readCSReals "resources/proper2.real_rgb.csv" )
        fun equal( truth, im ) =
          RealRGBImage.foldi RealRGBImage.RowMajor
            ( fn( i, j, ( r, g, b ), equal ) =>
              let
                val tr = Array.sub( truth, i*(width*3)+j*3 )
                val tg = Array.sub( truth, i*(width*3)+j*3+1 )
                val tb = Array.sub( truth, i*(width*3)+j*3+2 )
              in
                if equal andalso 
                   Util.approxEqReal( r, tr , 3 ) andalso
                   Util.approxEqReal( g, tg, 3 ) andalso
                   Util.approxEqReal( b, tb, 3 ) then
                  equal
                else
                  false
              end )
            true
            ( RealRGBImage.full im )       
      in
        [ equal( truth1, out1 ), equal( truth1, out2 ) ]
      end ,
    inputToString=
      fn( _, ( i, s ) ) =>
        "( " ^ RealRGBImage.toString i ^ ", " ^ s ^ " )" }
