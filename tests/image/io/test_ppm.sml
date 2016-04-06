(* 
* file: test_ppm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the PPM implementation.
*)

val _ = print"\n\n********** PNM Tests **********\n"

val _ = UnitTest.test( "Testing PPM.read",
  fn() => [
    Option.valOf( RealPPM.read("proper2.raw.ppm") ),
    Option.valOf( RealPPM.read("proper2.plain.ppm") ) ] ,
  fn[ im1, im2 ] => 
  let
    val ( height, width ) = RealRGBImage.dimensions im1
    val truth1 = 
      Array.fromList( TextFileUtil.readCSReals "proper2.real_rgb.csv" )
  in
    RealRGBImage.foldi RealRGBImage.RowMajor
      ( fn( i, j, ( r, g, b ), equal ) =>
        let
          val tr = Array.sub( truth1, i*(width*3)+j*3 )
          val tg = Array.sub( truth1, i*(width*3)+j*3+1 )
          val tb = Array.sub( truth1, i*(width*3)+j*3+2 )
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
      ( RealRGBImage.full im1 )       
    andalso
    RealRGBImage.foldi RealRGBImage.RowMajor
      ( fn( i, j, ( r, g, b ), equal ) =>
        let
          val tr = Array.sub( truth1, i*(width*3)+j*3 )
          val tg = Array.sub( truth1, i*(width*3)+j*3+1 )
          val tb = Array.sub( truth1, i*(width*3)+j*3+2 )
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
      ( RealRGBImage.full im2 )       
  end )

val _ = UnitTest.test( "Testing PPM.write",
  fn() => 
  let
    val im = Option.valOf( RealPPM.read("proper2.raw.ppm") ) 
    
    val _ = RealPPM.write( im, "output/output.plain.ppm" )
    val _ = RealPPM.write' ( PNM.rawPPM, 0w255 ) ( im, "output/output.raw.ppm" )

    val out1 = Option.valOf( RealPPM.read( "output/output.plain.ppm" ) )
    val out2 = Option.valOf( RealPPM.read( "output/output.raw.ppm" ) )
  in 
    [ out1, out2 ] 
  end ,
  fn[ im1, im2 ] => 
  let
    val ( height, width ) = RealRGBImage.dimensions im1
    val truth1 = 
      Array.fromList( TextFileUtil.readCSReals "proper2.real_rgb.csv" )
  in
    RealRGBImage.foldi RealRGBImage.RowMajor
      ( fn( i, j, ( r, g, b ), equal ) =>
        let
          val tr = Array.sub( truth1, i*(width*3)+j*3 )
          val tg = Array.sub( truth1, i*(width*3)+j*3+1 )
          val tb = Array.sub( truth1, i*(width*3)+j*3+2 )
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
      ( RealRGBImage.full im1 )       
    andalso
    RealRGBImage.foldi RealRGBImage.RowMajor
      ( fn( i, j, ( r, g, b ), equal ) =>
        let
          val tr = Array.sub( truth1, i*(width*3)+j*3 )
          val tg = Array.sub( truth1, i*(width*3)+j*3+1 )
          val tb = Array.sub( truth1, i*(width*3)+j*3+2 )
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
      ( RealRGBImage.full im2 )       
  end )

