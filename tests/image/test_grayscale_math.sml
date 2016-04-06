(* 
* file: test_grayscale_math.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validatet validate basic grayscale math 
* operations
*)

val _ = print"\n\n********** Grayscale math tests **********\n"

val _ = UnitTest.test( "Maximum real value",
  fn() => 
  let
    val im = RealGrayscaleImage.fromList [ [  3.2, 4.3, 1.2 ],
                                           [ ~3.2, 5.7, 4.7 ],
                                           [  3.5, 2.3, 5.2 ] ]
  in
    GrayscaleMath.maxReal im
  end,
  fn x => Real.==( x, 5.7 ) )

val _ = UnitTest.test( "Minimum real value",
  fn() => 
  let
    val im = RealGrayscaleImage.fromList [ [  3.2, 4.3, 1.2 ],
                                           [ ~3.2, 5.7, 4.7 ],
                                           [  3.5, 2.3, 5.2 ] ]
  in
    GrayscaleMath.minReal im
  end,
  fn x => Real.==( x, ~3.2 ) )

val _ = UnitTest.test( "Sum real value",
  fn() => 
  let
    val im = RealGrayscaleImage.fromList [ [  3.2, 4.3, 1.2 ],
                                           [ ~3.2, 5.7, 4.7 ],
                                           [  3.5, 2.3, 5.2 ] ]
  in
    GrayscaleMath.sumReal im
  end,
  fn x => Real.==( x, 26.9) )

val _ = UnitTest.test( "Mean real value",
  fn() => 
  let
    val im = RealGrayscaleImage.fromList [ [  3.2, 4.3, 1.2 ],
                                           [ ~3.2, 5.7, 4.7 ],
                                           [  3.5, 2.3, 5.2 ] ]
  in
    GrayscaleMath.meanReal im
  end,
  fn x => Util.approxEqReal( x, 2.9889, 3 ) )


