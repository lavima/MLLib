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
    group="Word8PGM", what="Loading, saving, and reloading PGM image as GrayscaleImageWord8.image",
    fn () => 
      let
        val im = Option.valOf( Word8PGM.read("simple.plain.pgm") )
        val _ = Word8PGM.write( im, "output2.pgm" )
        val loaded = Option.valOf( Word8PGM.read("output2.pgm") )
      in
        loaded
      end ,
    fn x => 
      if Word8GrayscaleImage.sub( x, 0, 0 )=0w0 andalso
         Word8GrayscaleImage.sub( x, 0, 1 )=0w64 andalso
         Word8GrayscaleImage.sub( x, 1, 0 )=0w128 andalso
         Word8GrayscaleImage.sub( x, 1, 1 )=0w255 then
        true
      else
        false )

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Loading, saving, and reloading PGM image as GrayscaleImageReal.image",
    fn() => 
      let
        val im = Option.valOf( RealPGM.read("simple.plain.pgm") )
        val _ = RealPGM.write( im, "output1.pgm" )
      in
        Option.valOf( RealPGM.read "output1.pgm" )
      end ,
    fn x => 
      if Real.==( RealGrayscaleImage.sub( x, 0, 0 ), 0.0 ) andalso
         Real.==( RealGrayscaleImage.sub( x, 0, 1 ), ( 64.0/255.0 ) ) andalso
         Real.==( RealGrayscaleImage.sub( x, 1, 0 ), ( 128.0/255.0 ) ) andalso
         Real.==( RealGrayscaleImage.sub( x, 1, 1 ), 1.0 ) then
        true
      else
        false )

val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Loading, saving and reloading PBM image as Boolean.image",
    fn() =>
      let
        val im = Option.valOf( BooleanPBM.read("simple.raw.pbm") )
        val _ = BooleanPBM.write( im, "output1.pbm" )
      in
        Option.valOf( BooleanPBM.read("output1.pbm") )
      end ,
    fn x =>
      if BooleanImage.sub( x, 0, 0 ) andalso
         not( BooleanImage.sub( x, 0, 1 ) ) andalso
         BooleanImage.sub( x, 1, 0 ) andalso
         not( BooleanImage.sub( x, 1, 1 ) ) then
        true
      else
        false )

val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Saving and reloading a simple binary image using RAW PBM",
    fn() =>
      let
        val im = BooleanImage.fromList'( 2, 2, [ true, false, true, false ] )
        val _ = BooleanPBM.write' PNM.rawPBM ( im, "output2.pbm" )
      in
        Option.valOf( BooleanPBM.read "output2.pbm" )
      end , 
    fn x =>
      if BooleanImage.sub( x, 0, 0 ) andalso
         not( BooleanImage.sub( x, 0, 1 ) ) andalso
         BooleanImage.sub( x, 1, 0 ) andalso
         not( BooleanImage.sub( x, 1, 1 ) ) then
        true
      else
        false )

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Testing histogram on small generated image",
    fn() => 
      let
        val im = 
          RealGrayscaleImage.fromList'( 3, 3, 
            [ 1.0, 0.5, 0.3, 0.54, 0.0, 0.1, 0.65, 0.56, 0.31 ] )
        val Histogram = RealGrayscaleHistogram.histogram' 4 im
      in
        Histogram
      end ,
    fn Histogram => 
    let
      val truth = 
        Array.fromList( [ 2, 2, 4, 1 ] )
    in
      Array.foldli 
        ( fn( I, x, t ) => 
            if t andalso x=Array.sub( truth, I ) then
              true
            else
              false ) 
        true
        Histogram
    end )

val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Testing histogram on proper grayscale image",
    fn() =>
    let
      val im = Option.valOf( RealPGM.read "proper.plain.pgm" )
      val Histogram = RealGrayscaleHistogram.histogram' 64 im
    in
      Histogram
    end ,
    fn Histogram => 
    let
      val truth = 
        Array.fromList( 
          [ 2247, 527, 335, 181, 98, 71, 47, 34, 
            34, 70, 191, 138, 168, 258, 406, 579, 
            1022, 22820, 39792, 18471, 3054, 727, 819, 1068, 
            1218, 1567, 1992, 2179, 2368, 2539, 2588, 2532, 
            2401, 2237, 2067, 1905, 1645, 1491, 1440, 1257, 
            1067, 833, 876, 617, 510, 475, 392, 378,
            439, 430, 427, 391, 417, 459, 442, 492, 
            509, 532, 497, 389, 115, 30, 0, 0 ] ) 
    in
      Array.foldli
        ( fn( I, x, Equal ) =>
            if Equal andalso x=Array.sub( truth, I ) then
              true
            else
              false )
        true
        Histogram
    end )

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Testing GrayscaleImageReal.correlate",
    fn() =>
    let
      val im = 
        RealGrayscaleImage.fromList'( 3, 3, 
          [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 ] )
      val Filter =
        RealGrayscaleImage.fromList'( 1, 2, [ 1.0, 2.0 ] )
    in
      RealGrayscaleImage.correlate 
        ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize )
        ( im, Filter )
    end ,
    fn x => 
    let
      val truth = 
        RealGrayscaleImage.fromList'( 3, 3, 
          [ 5.0, 8.0, 9.0, 14.0, 17.0, 18.0, 23.0, 26.0, 27.0 ] )
    in
      RealGrayscaleImage.equal( x, truth )
    end )

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Testing GrayscaleImageReal.convolve",
    fn() =>
    let
      val im1 = 
        RealGrayscaleImage.fromList'( 3, 3, 
          [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 ] )
      val filter1 =
        RealGrayscaleImage.fromList'( 1, 2, [ 1.0, 2.0 ] )

      val im2 = 
        RealGrayscaleImage.fromList(
          [ [ 1.0, 2.0, 3.0, 4.0, 5.0 ],
            [ 6.0, 7.0, 8.0, 9.0, 10.0 ],
            [ 11.0, 12.0, 13.0, 14.0, 15.0 ],
            [ 16.0, 17.0, 18.0, 19.0, 20.0 ],
            [ 21.0, 22.0, 23.0, 24.0, 25.0 ] ] )
      val filter2 =
        RealGrayscaleImage.fromList( 
          [ [ 10.0, 15.0, 11.0 ],
            [ 5.0, 3.0, 4.0 ],
            [ 21.0, 20.0, 19.0 ] ] )
    in
      [ 
        RealGrayscaleImage.convolve 
          ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize )
          ( im1, filter1 ),
        RealGrayscaleImage.convolve 
          ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize )
          ( im2, filter2 )
          
        ]
    end ,
    fn[ x, y ] => 
    let
      val truth1 = 
        RealGrayscaleImage.fromList'( 3, 3, 
          [ 4.0, 7.0, 9.0, 13.0, 16.0, 18.0, 22.0, 25.0, 27.0 ] )
      val truth2 = 
        RealGrayscaleImage.fromList(
          [ [ 324.0, 398.0, 506.0, 614.0, 686.0 ],
            [ 564.0, 638.0, 746.0, 854.0, 926.0 ],
            [ 1104.0, 1178.0, 1286.0, 1394.0, 1466.0 ],
            [ 1644.0, 1718.0, 1826.0, 1934.0, 2006.0 ],
            [ 2004.0, 2078.0, 2186.0, 2294.0, 2366.0 ] ] )
    in
      RealGrayscaleImage.equal( x, truth1 )
      andalso
      RealGrayscaleImage.equal( y, truth2 )
    end )

val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Testing Berkeley FMeasure",
    fn() => 
      let

        (* Test with simple generated image *)

        val im = 
          BooleanImage.fromList'( 3, 3, 
            [ true, false, false, false, true, false, false, false, true ] )
        val truth = 
          BooleanImage.fromList'( 3, 3, 
            [ true, false, false, false, true, false, false, true, false ] )
        val score1 = FMeasureBerkeley.evaluateEdge( im, [ truth ] )


        (* Test with proper edge classified image *)

        val edges = 
            Option.valOf( BooleanPBM.read( "edge_classified.plain.pbm" ) ) 
        val segs = 
            Option.valOf( IntPGM.read( "proper2.seg.raw.pgm" ) ) 

        val truths =
          List.map
            ( fn Filename => 
                Option.valOf( BooleanPBM.read Filename ) )
            [ "edge_truth_1.plain.pbm",
              "edge_truth_2.plain.pbm",
              "edge_truth_3.plain.pbm",
              "edge_truth_4.plain.pbm",
              "edge_truth_5.plain.pbm",
              "edge_truth_6.plain.pbm" ]

        val score2 = FMeasureBerkeley.evaluateEdge( edges, truths )

        val score3 = FMeasureBerkeley.evaluateSegmentation( segs, truths )

        (*val _ = print( FMeasureBerkeleyEdge.toString score1 ^ "\n" )*)
        (*val _ = print( FMeasureBerkeleyEdge.toString score2 ^ "\n" )*)
      in
        [ score1, score2, score3 ] 
      end ,
    fn[ score1 as ( _, _, _, _, _, _, F1), 
        score2 as ( _, _, _, _, _, _, F2 ),
        score3 as ( _, _, _, _, _, _, F3 ) ] => 
      (* Uncommment for a single evaluation *)
      Util.approxEqReal( F1, 0.666, 3 ) andalso 
      Util.approxEqReal( F2, 0.495956270745496, 2 ) andalso
      (* P = 0.352722029988466
         R = 0.835059185285489
         F = 0.495956270745496 *)
      Util.approxEqReal( F3, 0.767055072099290, 2 ) 
      (* P = 0.914462944825335
         R = 0.660573234032950
         F = 0.767055072099290
      *)
      )

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Testing morphological thinning",
    fn() =>
    let
      val t = true
      val f = false
      val im = 
        BooleanImage.fromList'( 5, 11, 
          [ t, t, t, t, t, t, t, t, t, t, t, 
            t, t, t, t, t, t, t, t, t, f, f,
            t, t, t, t, t, t, t, t, t, f, f,
            t, t, t, t, t, t, t, t, t, f, f,
            t, t, t, f, f, t, t, t, t, f, f ] )
    in
      Morphology.thin im
    end , 
    fn x => 
    let 
      val t = true
      val f = false
      val truth = 
        BooleanImage.fromList'( 5, 11, 
          [ t, f, f, f, f, f, f, f, t, t, t, 
            t, t, f, f, f, f, f, t, t, f, f, 
            f, t, t, t, t, t, t, t, f, f, f,
            t, t, f, f, f, f, f, t, f, f, f,
            t, f, f, f, f, f, f, t, t, f, f ] )
    in
      BooleanImage.equal( x, truth )       
    end )


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Rotating a image as GrayscaleImageReal.image",
    fn() => 
      let
        val Image = Option.valOf( RealPGM.read("test2.pgm") )
        val newImage = RealGrayscaleImage.rotate (Image, 1.5708) (* 90 deg.  *)
        val _ = RealPGM.write( newImage, "output/outputRotate1.pgm" )
      in
        Option.valOf( RealPGM.read("output/outputRotate1.pgm") )
      end ,
    fn( rotated ) =>
      RealGrayscaleImage.nRows rotated = 482 andalso 
      RealGrayscaleImage.nCols rotated = 322 )


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Word8PGM", what="Adding border using borderextension.mirror",
    fn() => 
      let
        val image = Option.valOf( RealPGM.read("test2.pgm") )
        val newImage = 
          RealGrayscaleImage.border 
            ( RealGrayscaleImage.MirrorExtension, 100 ) 
            image
      in
        newImage
      end ,
    fn( extended ) =>
      RealGrayscaleImage.nRows extended = 482 andalso 
      RealGrayscaleImage.nCols extended = 322 )

val _ =
  DifferentialTest.test' ( CommandLine.arguments() ) 
   {group = "Image", 
    what = "Rotate image",
    num = 10,
    genInput = 
      fn () => 
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
    inputToString = fn( file, rand ) => file
  } 
