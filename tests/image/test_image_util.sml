(* 
* file: test_filter_util.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the image utility functionality.
*)

val _ = print"\n\n********** Image utility tests **********\n"

local
  val imageData =  [ [ 0.11, 0.21, 0.35 ],
                     [ 0.42, 0.53, 0.62 ],
                     [ 0.73, 0.85, 0.91 ] ] 
in
  val _ = UnitTest.test( "Approximate real grayscale image comparison",
    fn() => RealGrayscaleImage.fromList imageData,
    fn x =>
    let
      val expectedTrue = RealGrayscaleImage.fromList imageData

      val expectedFalse = RealGrayscaleImage.fromList( 
                  [ [ 0.10, 0.21, 0.35 ],
                    [ 0.41, 0.53, 0.62 ],
                    [ 0.73, 0.82, 0.91 ] ] )
    in
      ImageUtil.approxCompareGrayscaleReal( x, expectedTrue, 2 ) andalso 
      not ( ImageUtil.approxCompareGrayscaleReal( x, expectedFalse, 2 ) )
    end )
end

local
  val lChannel = [ [ 0.11, 0.21, 0.35 ],
                   [ 0.42, 0.53, 0.62 ],
                   [ 0.73, 0.85, 0.91 ] ]

  val aChannel = [ [ 0.21, 0.61, 0.95 ],
                   [ 0.52, 0.73, 0.72 ],
                   [ 0.43, 0.15, 0.41 ] ]

  val bChannel = [ [ 0.71, 0.41, 0.65 ],
                   [ 0.52, 0.23, 0.52 ],
                   [ 0.33, 0.05, 0.31 ] ]

  val cieImageData = 
    [ [ ( 0.11, 0.21, 0.71 ), ( 0.21, 0.61, 0.41 ), ( 0.35, 0.95, 0.65 ) ],
      [ ( 0.42, 0.52, 0.52 ), ( 0.53, 0.73, 0.23 ), ( 0.62, 0.72, 0.52 ) ],
      [ ( 0.73, 0.43, 0.33 ), ( 0.85, 0.15, 0.05 ), ( 0.91, 0.41, 0.31 ) ] ]

in

  val _ = UnitTest.test( "get A channel from real CIE lab",
    fn() => 
      ImageUtil.getAChannel ( RealCIELabImage.fromList cieImageData ),
    fn x =>
    let
      val expected = RealGrayscaleImage.fromList aChannel
    in
      RealGrayscaleImage.equal ( x, expected )
    end )

  val _ = UnitTest.test( "get B channel from real CIE lab",
    fn() => 
      ImageUtil.getBChannel ( RealCIELabImage.fromList cieImageData ),
    fn x =>
    let
      val expected = RealGrayscaleImage.fromList bChannel
    in
      RealGrayscaleImage.equal ( x, expected )
    end )

  val _ = UnitTest.test( "get L channel from real CIE lab",
    fn() => 
      ImageUtil.getLChannel ( RealCIELabImage.fromList cieImageData ),
    fn x =>
    let
      val expected = RealGrayscaleImage.fromList lChannel
    in
      RealGrayscaleImage.equal ( x, expected )
    end )


end
