(* 
* file: test_filter_convert.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate image conversion functionality.
*)


val _ = print"\n\n********** Image conversion tests **********\n"

val _ = UnitTest.test( "Convert real RGB image to grayscale",
  fn() => 
    let
      val imageData =  [ [ ( 0.11, 0.32, 0.23 ), ( 0.21, 0.13, 0.42 ) ],
                         [ ( 0.42, 0.31, 0.23 ), ( 0.53, 0.65, 0.38 ) ] ] 
  
      val image = RealRGBImage.fromList imageData
    in
      ImageConvert.realRGBtoGray image
    end,
  fn x =>
  let
    val expectedData = [ [ 0.2469608 , 0.186981 ],
                         [ 0.3337618 , 0.583342 ] ]

    val expected = RealGrayscaleImage.fromList expectedData
    
  in
    ImageUtil.approxCompareGrayscaleReal ( expected, x, 4 )
  end )
