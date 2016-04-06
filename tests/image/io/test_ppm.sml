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
    val truth1 = Array.fromList( TextFileUtil.readCSReals "proper2.real_rgb.csv" )
  in
    RealRGBImage.foldi RealRGBImage.RowMajor
      ( fn( i, j, ( r, g, b ), equal ) =>
          if equal andalso 
             Util.approxEqReal( r, Array.sub( truth1, i*(width*3)+j*3 ), 3 ) andalso
             Util.approxEqReal( g, Array.sub( truth1, i*(width*3)+j*3+1 ), 3 ) andalso
             Util.approxEqReal( b, Array.sub( truth1, i*(width*3)+j*3+2 ), 3 ) then
            equal
          else
            false )
      true
      ( RealRGBImage.full im1 )       
    andalso
    RealRGBImage.foldi RealRGBImage.RowMajor
      ( fn( i, j, ( r, g, b ), equal ) =>
          if equal andalso 
             Util.approxEqReal( r, Array.sub( truth1, i*(width*3)+j*3 ), 3 ) andalso
             Util.approxEqReal( g, Array.sub( truth1, i*(width*3)+j*3+1 ), 3 ) andalso
             Util.approxEqReal( b, Array.sub( truth1, i*(width*3)+j*3+2 ), 3 ) then
            equal
          else
            false )
      true
      ( RealRGBImage.full im2 )       
  end )

