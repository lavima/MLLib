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

val rgbreq = rgbeq Util.eqReal
val rgbweq = rgbeq ( Util.eq Word8.compare )



val _ = print"\n\n********** Image Tests **********\n"

val _ = test( "Loading PGM image as GrayscaleImageWord8.image",
  fn() => Option.valOf( GrayscaleImageWord8.load("simple.plain.pgm") ),
  fn X => 
    if GrayscaleImageWord8.sub( X, 0, 0 )=0w0 andalso
       GrayscaleImageWord8.sub( X, 1, 0 )=0w64 andalso
       GrayscaleImageWord8.sub( X, 0, 1 )=0w128 andalso
       GrayscaleImageWord8.sub( X, 1, 1 )=0w255 then
      true
    else
      false )

val _ = test( "Loading PGM image as GrayscaleImageReal.image",
  fn() => Option.valOf( GrayscaleImageReal.load("simple.plain.pgm") ),
  fn X => 
    if Util.eqReal( GrayscaleImageReal.sub( X, 0, 0 ), 0.0 ) andalso
       Util.eqReal( GrayscaleImageReal.sub( X, 1, 0 ), ( 64.0/255.0 ) ) andalso
       Util.eqReal( GrayscaleImageReal.sub( X, 0, 1 ), ( 128.0/255.0 ) ) andalso
       Util.eqReal( GrayscaleImageReal.sub( X, 1, 1 ), 1.0 ) then
      true
    else
      false )

val _ = test( "Loading simple RAW PGM image as GrayscaleImageReal.image",
  fn() => Option.valOf( GrayscaleImageReal.load("simple.raw.pgm") ),
  fn X => 
    if Util.eqReal( GrayscaleImageReal.sub( X, 0, 0 ), 0.0 ) andalso
       Util.eqReal( GrayscaleImageReal.sub( X, 1, 0 ), ( 64.0/255.0 ) ) andalso
       Util.eqReal( GrayscaleImageReal.sub( X, 0, 1 ), ( 128.0/255.0 ) ) andalso
       Util.eqReal( GrayscaleImageReal.sub( X, 1, 1 ), 1.0 ) then
      true
    else
      false )

val _ = test( "Loading proper PGM image as GrayscaleImageReal.image",
  fn() => Option.valOf( GrayscaleImageReal.load("proper.plain.pgm") ),
  fn Image => 
  let
    val Truth = Array.fromList( TextFileUtil.readCSReals "proper.real.csv" )
  in
    GrayscaleImageReal.foldli
      ( fn( I, X, Equal ) =>
          if Equal andalso 
             Util.approxEqReal( X, Array.sub( Truth, I ), 3 ) then
            Equal
          else
            false )
      true
      Image        
  end )

val _ =
  test( "Loading a simple PBM",
    fn() =>
      Option.valOf( BooleanImage.load( "simple.plain.pbm" ) ) ,
    fn X =>
      if BooleanImage.sub( X, 0, 0 ) andalso
         not( BooleanImage.sub( X, 1, 0 ) ) andalso
         BooleanImage.sub( X, 0, 1 ) andalso
         not( BooleanImage.sub( X, 1, 1 ) ) then
        true
      else
        false )

val _ =
  test( "Loading a simple RAW PBM",
    fn() =>
      Option.valOf( BooleanImage.load( "simple.raw.pbm" ) ) ,
    fn X =>
      if BooleanImage.sub( X, 0, 0 ) andalso
         not( BooleanImage.sub( X, 1, 0 ) ) andalso
         BooleanImage.sub( X, 0, 1 ) andalso
         not( BooleanImage.sub( X, 1, 1 ) ) then
        true
      else
        false )

val _ = test( "Loading proper RAW PBM image as BooleanImage.image",
  fn() => Option.valOf( BooleanImage.load("proper.raw.pbm") ),
  fn X => true )

val _ = 
  test( "Loading, saving, and reloading PGM image as GrayscaleImageWord8.image",
    fn () => 
      let
        val Image = Option.valOf( GrayscaleImageWord8.load("simple.plain.pgm") )
        val _ = GrayscaleImageWord8.save( Image, "output1.pgm" )
        val Loaded = Option.valOf( GrayscaleImageWord8.load("output1.pgm") )
      in
        Loaded
      end ,
    fn X => 
      if GrayscaleImageWord8.sub( X, 0, 0 )=0w0 andalso
         GrayscaleImageWord8.sub( X, 1, 0 )=0w64 andalso
         GrayscaleImageWord8.sub( X, 0, 1 )=0w128 andalso
         GrayscaleImageWord8.sub( X, 1, 1 )=0w255 then
        true
      else
        false )

val _ = 
  test( "Loading, saving, and reloading PGM image as GrayscaleImageReal.image",
    fn() => 
      let
        val Image = Option.valOf( GrayscaleImageReal.load("simple.plain.pgm") )
        val _ = GrayscaleImageReal.save( Image, "output1.pgm" )
      in
        Option.valOf( GrayscaleImageReal.load("output1.pgm") )
      end ,
    fn X => 
      if Util.eqReal( GrayscaleImageReal.sub( X, 0, 0 ), 0.0 ) andalso
         Util.eqReal( GrayscaleImageReal.sub( X, 1, 0 ), ( 64.0/255.0 ) ) andalso
         Util.eqReal( GrayscaleImageReal.sub( X, 0, 1 ), ( 128.0/255.0 ) ) andalso
         Util.eqReal( GrayscaleImageReal.sub( X, 1, 1 ), 1.0 ) then
        true
      else
        false )

val _ =
  test( "Loading, saving and reloading PBM image as Boolean.image",
    fn() =>
      let
        val Image = Option.valOf( BooleanImage.load("simple.raw.pbm") )
        val _ = BooleanImage.save( Image, "output1.pbm" )
      in
        Option.valOf( BooleanImage.load("output1.pbm") )
      end ,
    fn X =>
      if BooleanImage.sub( X, 0, 0 ) andalso
         not( BooleanImage.sub( X, 1, 0 ) ) andalso
         BooleanImage.sub( X, 0, 1 ) andalso
         not( BooleanImage.sub( X, 1, 1 ) ) then
        true
      else
        false )

val _ =
  test( "Saving and reloading a simple binary image using RAW PBM",
    fn() =>
      let
        val Image = BooleanImage.fromList( 2, 2, [ true, false, true, false ] )
        val _ = BooleanImage.save' ( PNMCommon.rawPBM, 0w1 ) 
                                   ( Image, "output2.pbm" )

      in
        Option.valOf( BooleanImage.load( "output2.pbm" ) )
      end , 
    fn X =>
      if BooleanImage.sub( X, 0, 0 ) andalso
         not( BooleanImage.sub( X, 1, 0 ) ) andalso
         BooleanImage.sub( X, 0, 1 ) andalso
         not( BooleanImage.sub( X, 1, 1 ) ) then
        true
      else
        false )

val _ = 
  test( "Testing histogram on small generated image",
    fn() => 
      let
        val Image = 
          GrayscaleImageReal.fromList( 3, 3, 
            [ 1.0, 0.5, 0.3, 0.54, 0.0, 0.1, 0.65, 0.56, 0.31 ] )
        val [ Histogram ] = GrayscaleImageReal.histograms( Image, 4 )
      in
        Histogram
      end ,
    fn Histogram => 
    let
      val Truth = 
        Array.fromList( [ 2, 2, 4, 1 ] )
    in
      Array.foldli 
        ( fn( I, X, T ) => 
            if T andalso X=Array.sub( Truth, I ) then
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
      val Image = Option.valOf( GrayscaleImageReal.load "proper.plain.pgm" )
      val [ Histogram ] = GrayscaleImageReal.histograms( Image, 64 )
    in
      Histogram
    end ,
    fn Histogram => 
    let
      val Truth = 
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
        ( fn( I, X, Equal ) =>
            if Equal andalso X=Array.sub( Truth, I ) then
              true
            else
              false )
        true
        Histogram
    end )

val _ = 
  test( "Testing GrayscaleImageReal.correlate",
    fn() =>
    let
      val Image = 
        GrayscaleImageReal.fromList( 3, 3, 
          [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 ] )
      val Filter =
        GrayscaleImageReal.fromList( 2, 1, [ 1.0, 2.0 ] )
    in
      GrayscaleImageReal.correlate ( ImageCommon.copy, ImageCommon.original )
        ( Image, Filter )
    end ,
    fn X => 
    let
      val Truth = 
        GrayscaleImageReal.fromList( 3, 3, 
          [ 5.0, 8.0, 9.0, 14.0, 17.0, 18.0, 23.0, 26.0, 27.0 ] )
    in
      GrayscaleImageReal.equal( X, Truth )
    end )

val _ = 
  test( "Testing GrayscaleImageReal.convolve",
    fn() =>
    let
      val Image = 
        GrayscaleImageReal.fromList( 3, 3, 
          [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 ] )
      val Filter =
        GrayscaleImageReal.fromList( 2, 1, [ 1.0, 2.0 ] )
    in
      GrayscaleImageReal.convolve ( ImageCommon.copy, ImageCommon.original )
        ( Image, Filter )
    end ,
    fn X => 
    let
      val Truth = 
        GrayscaleImageReal.fromList( 3, 3, 
          [ 4.0, 7.0, 9.0, 13.0, 16.0, 18.0, 22.0, 25.0, 27.0 ] )
    in
      GrayscaleImageReal.equal( X, Truth )
    end )

val _ =
  test( "Testing Berkeley FMeasure for edges",
    fn() => 
      let

        (* Test with simple generated image *)

        val Image = 
          BooleanImage.fromList( 3, 3, 
            [ true, false, false, false, true, false, false, false, true ] )
        val Truth = 
          BooleanImage.fromList( 3, 3, 
            [ true, false, false, false, true, false, false, true, false ] )
        val Score1 = FMeasureBerkeleyEdge.evaluate( Image, [ Truth ] )


        (* Test with with proper edge classified image *)

        (* The following code generates an edge classified image by running
           the Canny detector on the first image in the BSDS. It saves a copy of
           the result and the corresponding ground truths. *)
        (*
        val DataDir = "../../Research/imageanalysis/data/BSDS_PNM/"
        val ImageDir = DataDir ^ "images/"
        val TruthDir = DataDir ^ "truth/"
        val FilenamesFile = "bsds.train.grayscale.filenames"
        val TruthFilenamesFile = "bsds.train.truth.filenames"

        val FirstFile::_ = TextFileUtil.readFilenames( DataDir ^ FilenamesFile )
        val FirstTruthFile::_ = TextFileUtil.readFilenames( DataDir ^ TruthFilenamesFile )
        val FirstFileName = OS.Path.base FirstFile
        val Image = 
          Option.valOf( GrayscaleImageReal.load( ImageDir ^ FirstFile ) )
        val Edges = Canny.findEdges' ( Canny.otsuHighLowRatio 0.8 ) Image

        val _ = 
          GrayscaleImageReal.save( 
            ImageUtil.convertBooleanToReal Edges, 
            FirstFileName ^ ".pgm" )

        val TruthFilenames = 
          TextFileUtil.readFilenames( TruthDir ^ FirstTruthFile )

        val TruthImages = 
          List.map
            ( fn TruthImageFilename => 
                Option.valOf( BooleanImage.load( TruthDir ^ TruthImageFilename ) ) )
            TruthFilenames 

        val _ = 
          List.app
            ( fn( Truth, Filename ) => 
                BooleanImage.save( Truth, Filename ) )
            ( ListUtil.combine( TruthImages, TruthFilenames ) ) 
        *)

        val Edges = 
            Option.valOf( BooleanImage.load( "edge_classified.plain.pgm" ) ) 

        val TruthImages =
          List.map
            ( fn Filename => 
                Option.valOf( BooleanImage.load( Filename ) ) )
            [ "edge_truth_1.plain.pbm",
              "edge_truth_2.plain.pbm",
              "edge_truth_3.plain.pbm",
              "edge_truth_4.plain.pbm",
              "edge_truth_5.plain.pbm",
              "edge_truth_6.plain.pbm" ]

        (* Uncomment for a single evaluation *)
        val Score2 = FMeasureBerkeleyEdge.evaluate( Edges, TruthImages )
        (* Uncomment for a full statistical comparison *)
        (*val EvalList = List.tabulate( 1000, fn _ => ( Edges, TruthImages ) )
        val Score2 = FMeasureBerkeleyEdge.evaluateList( EvalList )*)
        (*val _ = print( FMeasureBerkeleyEdge.toString Score1 ^ "\n" )*)
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
      val T = true
      val F = false
      val Image = 
        BooleanImage.fromList( 11, 5, 
          [ T, T, T, T, T, T, T, T, T, T, T, 
            T, T, T, T, T, T, T, T, T, F, F,
            T, T, T, T, T, T, T, T, T, F, F,
            T, T, T, T, T, T, T, T, T, F, F,
            T, T, T, F, F, T, T, T, T, F, F ] )
    in
      Morphology.thin Image
    end , 
    fn X => 
    let 
      val T = true
      val F = false
      val Truth = 
        BooleanImage.fromList( 11, 5, 
          [ T, F, F, F, F, F, F, F, T, T, T, 
            T, T, F, F, F, F, F, T, T, F, F, 
            F, T, T, T, T, T, T, T, F, F, F,
            T, T, F, F, F, F, F, T, F, F, F,
            T, F, F, F, F, F, F, T, T, F, F ] )
    in
      BooleanImage.equal( X, Truth )       
    end )


val _ = 
  test( "Rotating a image as GrayscaleImageReal.image",
    fn() => 
      let
        val Image = Option.valOf( GrayscaleImageReal.load("test2.pgm") )
        val newImage = GrayscaleImageReal.rotate (Image, 1.5708) (* 90 deg.  *)
        val _ = GrayscaleImageReal.save( newImage, "outputRotate1.pgm" )
      in
        Option.valOf( GrayscaleImageReal.load("output1.pgm") )
      end ,
    fn { height, width, ...} => height = 482 andalso width = 322 )


val _ = 
  test( "Adding border using borderextension.mirror",
    fn() => 
      let
        val image = Option.valOf( GrayscaleImageReal.load("test2.pgm") )
        val newImage = GrayscaleImageReal.border (ImageCommon.mirror, 100) image
        val _ = GrayscaleImageReal.save( newImage, "output/outputBorderMirror.pgm" )
      in
        Option.valOf( GrayscaleImageReal.load("output1.pgm") )
      end ,
    fn { height, width, ...} => height = 482 andalso width = 322 )


