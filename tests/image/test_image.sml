(* 
* file: test_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the image library implementation.
*)



fun rgbeq ( eq : 'a * 'a -> bool )
          ( P1 as ( R1, G1, B1 ) : 'a * 'a * 'a, 
            P2 as ( R2, G2, B2 ) : 'a * 'a * 'a ) : bool =
  ( eq( R1, R2 ) andalso eq( G1, G2 ) andalso eq( B1, B2 ) )

val rgbreq = rgbeq Real.==
val rgbweq = rgbeq ( fn( x : Word8.word, y : Word8.word ) => x=y ) 


val _ = print"\n\n********** Image Tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealGrayscaleImage", what="correlate",
    genInput= 
      fn() => [
        ( RealGrayscaleImage.fromList[ 
            [ 1.0, 2.0, 3.0 ], 
            [ 4.0, 5.0, 6.0 ], 
            [ 7.0, 8.0, 9.0 ] ],
          RealGrayscaleImage.fromList[ [ 1.0, 2.0 ] ] ) ] ,
    f= 
      fn[ i1 ] => [
        RealGrayscaleImage.correlate 
          ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize )
          ( #1 i1, #2 i1 ) ] ,
    evaluate=
      fn[ o1 ] => 
      let
        val truth = 
          RealGrayscaleImage.fromList[ 
            [ 5.0, 8.0, 9.0 ], 
            [ 14.0, 17.0, 18.0 ], 
            [ 23.0, 26.0, 27.0 ] ] 
      in
        [ RealGrayscaleImage.equal( o1, truth ) ]
      end ,
    inputToString= 
      fn( i, m ) => 
        "( " ^
        RealGrayscaleImage.toString i ^ ", " ^
        RealGrayscaleImage.toString m ^ 
        " )" }

 val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealGrayscaleImage", what="convolve",
    genInput= 
      fn() => [
        ( RealGrayscaleImage.fromList[ 
            [ 1.0, 2.0, 3.0 ], 
            [ 4.0, 5.0, 6.0 ], 
            [ 7.0, 8.0, 9.0 ] ],
          RealGrayscaleImage.fromList[ [ 1.0, 2.0 ] ] ) ,
        ( RealGrayscaleImage.fromList[ 
            [ 1.0, 2.0, 3.0, 4.0, 5.0 ],
            [ 6.0, 7.0, 8.0, 9.0, 10.0 ],
            [ 11.0, 12.0, 13.0, 14.0, 15.0 ],
            [ 16.0, 17.0, 18.0, 19.0, 20.0 ],
            [ 21.0, 22.0, 23.0, 24.0, 25.0 ] ],
          RealGrayscaleImage.fromList[ 
            [ 10.0, 15.0, 11.0 ],
            [ 5.0, 3.0, 4.0 ],
            [ 21.0, 20.0, 19.0 ] ] ), 
        ( RealGrayscaleImage.fromList[ [ 1.0, 2.0, 3.0 ] ],
          RealGrayscaleImage.fromList[ [ 15.0, 11.0, 17.0 ] ] ) ] ,
    f= 
      fn[ i1, i2, i3 ] => [
        RealGrayscaleImage.convolve 
          ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize )
          ( #1 i1, #2 i1 ),
        RealGrayscaleImage.convolve 
          ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize )
          ( #1 i2, #2 i2 ),
        RealGrayscaleImage.convolve 
          ( RealGrayscaleImage.ZeroExtension, RealGrayscaleImage.OriginalSize )
          ( #1 i3, #2 i3 ) ] ,
    evaluate=
      fn[ o1, o2, o3 ] => 
      let
        val truth1 = 
          RealGrayscaleImage.fromList[ 
            [ 4.0, 7.0, 9.0 ], 
            [ 13.0, 16.0, 18.0 ], 
            [ 22.0, 25.0, 27.0 ] ]
        val truth2 = 
          RealGrayscaleImage.fromList(
            [ [ 324.0, 398.0, 506.0, 614.0, 686.0 ],
              [ 564.0, 638.0, 746.0, 854.0, 926.0 ],
              [ 1104.0, 1178.0, 1286.0, 1394.0, 1466.0 ],
              [ 1644.0, 1718.0, 1826.0, 1934.0, 2006.0 ],
              [ 2004.0, 2078.0, 2186.0, 2294.0, 2366.0 ] ] )
        val truth3 = 
          RealGrayscaleImage.fromList[ [ 41.0, 84.0, 67.0 ] ]
      in 
        [ RealGrayscaleImage.equal( o1, truth1 ), 
          RealGrayscaleImage.equal( o2, truth2 ),
          RealGrayscaleImage.equal( o3, truth3 ) ]
      end ,
    inputToString= 
      fn( i, m ) => 
        "( " ^
        RealGrayscaleImage.toString i ^ ", " ^
        RealGrayscaleImage.toString m ^ 
        " )" }


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealGrayscaleImage", what="border",
    genInput= 
      fn() =>
        [ ( Option.valOf( RealPGM.read("test2.pgm") ),
            ( RealGrayscaleImage.MirrorExtension, 100 ) ) ] ,
    f= fn[ i1 ] => [ RealGrayscaleImage.border ( #2 i1 ) ( #1 i1 ) ] ,
    evaluate=
      fn[ o1 ] => 
        [ RealGrayscaleImage.nRows o1 = 482 andalso 
          RealGrayscaleImage.nCols o1 = 322 ] ,
    inputToString=
      fn( i, ( e, n ) ) =>
        "( " ^
        RealGrayscaleImage.toString i ^ ", " ^
        Int.toString n ^ 
        " )" }

(*
val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealGrayscaleImage", what="rotate",
    genInput=
      fn() => [ ( Option.valOf( RealPGM.read("test2.pgm") ), 1.5708 ) ] ,
    f= 
      fn[ i1 ] => [ RealGrayscaleImage.rotate i1 ] ,
    fn[ o1 ] =>
      [ RealGrayscaleImage.nRows rotated = 482 andalso 
        RealGrayscaleImage.nCols rotated = 322 ] ,
    inputToString=
      fn( i, a ) =>
        "( " ^
        RealGrayscaleImage.toString i ^ ", " ^
        Real.toString a ^ 
        " )" }
*)
val _ =
  DifferentialTest.test' ( CommandLine.arguments() ) {
    group = "RealGrayscaleImage", what = "rotate",
    num = 10,
    genInput = 
      fn _ => 
         (RandomArgumentUtilities.randomBSRImage (), 
          RandomArgumentUtilities.randomDecimal(0.0, Math.pi * 2.0)),
    fs = [
      fn (imageFile, rad) => 
      let
        val image = Option.valOf( RealPGM.read imageFile )
        val rotated = RealGrayscaleImage.rotate( image, rad )
      in
        rotated
      end,
      fn( imageFile, rad ) =>
      let 
        val fitest_image_rotate  = _import"fitest_image_rotate" : 
          real Array.array * real Array.array * 
          Word64.word * Word64.word * Word64.word * Word64.word * real -> unit;

        val image = Option.valOf( RealPGM.read imageFile )
        val ( height, width ) = RealGrayscaleImage.dimensions image

        val newWidth = Real.ceil(Real.max(
          abs((real width) * Math.cos(rad) - (real height) * Math.sin(rad)),
          abs((real width) * Math.cos(rad) + (real height) * Math.sin(rad))))
        val newHeight = Real.ceil(Real.max(
          abs((real width) * Math.sin(rad) - (real height) * Math.cos(rad)),
          abs((real width) * Math.sin(rad) + (real height) * Math.cos(rad))))

        val srcArray = Array.array( width*height, 0.0 ) 
        val _ = 
          RealGrayscaleImage.appi RealGrayscaleImage.ColMajor
            ( fn( i, j, x ) =>
                Array.update( srcArray, j*height+i, x ) )
            ( RealGrayscaleImage.full image )

        val destArray = Array.array( newWidth*newHeight, 0.0 )
         
        val _ = fitest_image_rotate 
          (srcArray,
           destArray, 
           Word64.fromInt width, 
           Word64.fromInt height, 
           Word64.fromInt newWidth, 
           Word64.fromInt newHeight,
           rad);

        val dest = 
          RealGrayscaleImage.tabulate RealGrayscaleImage.RowMajor ( 
            newHeight, newWidth, 
            fn( i, j ) => Array.sub( destArray, j*newHeight+i ) )
      in
        dest
      end
    ],
    compare = RealGrayscaleImage.equal,
    inputToString = fn( file, rand ) => file } 

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealGrayscaleImage", what="listFoldl",
    genInput=
      fn() => 
        [ [ RealGrayscaleImage.fromList[ [ 0.1, 0.8 ], [ 0.2, 0.9 ] ],
            RealGrayscaleImage.fromList[ [ 0.2, 0.7 ], [ 0.2, 0.5 ] ] ] ] ,
    f=
      fn[ i1 ] => 
        [ RealGrayscaleImage.listFoldl RealGrayscaleImage.RowMajor
            Util.realMax2 
            ( RealGrayscaleImage.zeroImage( 2, 2 ) )
            i1 ] ,
    evaluate=
      fn[ o1 ] =>
      let
        val truth = RealGrayscaleImage.fromList[ [ 0.2, 0.8 ], [ 0.2, 0.9 ] ]
      in
        [ RealGrayscaleImage.equal( o1, truth ) ]
      end ,
    inputToString= ListUtil.toString RealGrayscaleImage.toString }
