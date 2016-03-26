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

val _ = test( "Loading PGM image as Word8GrayscaleImage.image",
  fn() => Option.valOf( Word8PGM.read("simple.plain.pgm") ),
  fn im => 
    if Word8GrayscaleImage.sub( im, 0, 0 )=0w0 andalso
       Word8GrayscaleImage.sub( im, 0, 1 )=0w64 andalso
       Word8GrayscaleImage.sub( im, 1, 0 )=0w128 andalso
       Word8GrayscaleImage.sub( im, 1, 1 )=0w255 then
      true
    else
      false )

val _ = test( "Loading PGM image as RealGrayscaleImage.image",
  fn() => Option.valOf( RealPGM.read("simple.plain.pgm") ),
  fn im => 
    if Real.==( RealGrayscaleImage.sub( im, 0, 0 ), 0.0 ) andalso
       Real.==( RealGrayscaleImage.sub( im, 0, 1 ), ( 64.0/255.0 ) ) andalso
       Real.==( RealGrayscaleImage.sub( im, 1, 0 ), ( 128.0/255.0 ) ) andalso
       Real.==( RealGrayscaleImage.sub( im, 1, 1 ), 1.0 ) then
      true
    else
      false )

val _ = test( "Loading simple RAW PGM image as RealGrayscaleImage.image",
  fn() => Option.valOf( RealPGM.read("simple.raw.pgm") ),
  fn im => 
    if Real.==( RealGrayscaleImage.sub( im, 0, 0 ), 0.0 ) andalso
       Real.==( RealGrayscaleImage.sub( im, 0, 1 ), ( 64.0/255.0 ) ) andalso
       Real.==( RealGrayscaleImage.sub( im, 1, 0 ), ( 128.0/255.0 ) ) andalso
       Real.==( RealGrayscaleImage.sub( im, 1, 1 ), 1.0 ) then
      true
    else
      false )

val _ = test( "Loading proper PGM image as RealGrayscaleImage.image",
  fn() => Option.valOf( RealPGM.read("proper.plain.pgm") ),
  fn im => 
  let
    val ( height, width ) = RealGrayscaleImage.dimensions im
    val truth = Array.fromList( TextFileUtil.readCSReals "proper.real.csv" )
  in
    RealGrayscaleImage.foldi RealGrayscaleImage.RowMajor
      ( fn( i, j, x, equal ) =>
          if equal andalso 
             Util.approxEqReal( x, Array.sub( truth, i*width+j ), 3 ) then
            equal
          else
            false )
      true
      ( RealGrayscaleImage.full im )       
  end )

val _ =
  test( "Loading a simple PBM",
    fn() =>
      Option.valOf( BooleanPBM.read( "simple.plain.pbm" ) ) ,
    fn x =>
      if BooleanImage.sub( x, 0, 0 ) andalso
         not( BooleanImage.sub( x, 0, 1 ) ) andalso
         BooleanImage.sub( x, 1, 0 ) andalso
         not( BooleanImage.sub( x, 1, 1 ) ) then
        true
      else
        false )

val _ =
  test( "Loading a simple RAW PBM",
    fn() =>
      Option.valOf( BooleanPBM.read( "simple.raw.pbm" ) ) ,
    fn x =>
      if BooleanImage.sub( x, 0, 0 ) andalso
         not( BooleanImage.sub( x, 0, 1 ) ) andalso
         BooleanImage.sub( x, 1, 0 ) andalso
         not( BooleanImage.sub( x, 1, 1 ) ) then
        true
      else
        false )

val _ = test( "Loading proper RAW PBM image as BooleanImage.image",
  fn() => Option.valOf( BooleanPBM.read("proper.raw.pbm") ),
  fn x => true )

val _ = 
  test( "Loading, saving, and reloading PGM image as Word8GrayscaleImage.image",
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
  test( "Loading, saving, and reloading PGM image as RealGrayscaleImage.image",
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
  test( "Loading, saving and reloading PBM image as Boolean.image",
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
  test( "Saving and reloading a simple binary image using RAW PBM",
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
  test( "Testing histogram on small generated image",
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
  test( "Testing histogram on proper grayscale image",
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
  test( "Testing RealGrayscaleImage.correlate",
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
  test( "Testing RealGrayscaleImage.convolve",
    fn() =>
    let
      val im = 
        RealGrayscaleImage.fromList'( 3, 3, 
          [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 ] )
      val Filter =
        RealGrayscaleImage.fromList'( 1, 2, [ 1.0, 2.0 ] )
    in
      RealGrayscaleImage.convolve 
        ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize )
        ( im, Filter )
    end ,
    fn x => 
    let
      val truth = 
        RealGrayscaleImage.fromList'( 3, 3, 
          [ 4.0, 7.0, 9.0, 13.0, 16.0, 18.0, 22.0, 25.0, 27.0 ] )
    in
      RealGrayscaleImage.equal( x, truth )
    end )

val _ =
  test( "Testing Berkeley FMeasure for edges",
    fn() => 
      let

        (* Test with simple generated image *)

        val im = 
          BooleanImage.fromList'( 3, 3, 
            [ true, false, false, false, true, false, false, false, true ] )
        val truth = 
          BooleanImage.fromList'( 3, 3, 
            [ true, false, false, false, true, false, false, true, false ] )
        val Score1 = FMeasureBerkeleyEdge.evaluate( im, [ truth ] )

        (* Test with proper edge classified image *)

        val Edges = 
            Option.valOf( BooleanPBM.read( "edge_classified.plain.pbm" ) ) 

        val TruthImages =
          List.map
            ( fn Filename => 
                Option.valOf( BooleanPBM.read Filename ) )
            [ "edge_truth_1.plain.pbm",
              "edge_truth_2.plain.pbm",
              "edge_truth_3.plain.pbm",
              "edge_truth_4.plain.pbm",
              "edge_truth_5.plain.pbm",
              "edge_truth_6.plain.pbm" ]

        val Score2 = FMeasureBerkeleyEdge.evaluate( Edges, TruthImages )

        val _ = print( FMeasureBerkeleyEdge.toString Score1 ^ "\n" )
        val _ = print( FMeasureBerkeleyEdge.toString Score2 ^ "\n" )
      in
        [ Score1, Score2 ] 
      end ,
    fn[ Score1 as ( _, _, _, _, _, _, F1), Score2 as ( _, _, _, _, _, _, F2 ) ] => 
      (* Uncommment for a single evaluation *)
      Util.approxEqReal( F1, 0.666, 3 ) andalso 
      Util.approxEqReal( F2, 0.495956270745496, 2 ) 
      (* P = 0.352722029988466
         R = 0.835059185285489
         F = 0.495956270745496 *)
      )

val _ = 
  test( "Testing morphological thinning",
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
  test( "Rotating a image as RealGrayscaleImage.image",
    fn() => 
      let
        val im = Option.valOf( RealPGM.read "test2.pgm" )
        val newImage = RealGrayscaleImage.rotate( im, 1.5708 ) (* 90 deg.  *)
        val _ = RealPGM.write( newImage, "outputRotate1.pgm" )
      in
        Option.valOf( RealPGM.read("output1.pgm") )
      end ,
    fn( rotated ) =>
      RealGrayscaleImage.nRows rotated = 482 andalso 
      RealGrayscaleImage.nCols rotated = 322 )


