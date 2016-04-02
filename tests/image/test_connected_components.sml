(*
* file: test_connected-component.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests connected components functionality.
* 
*)

val _ = print"\n\n********** Connected components tests **********\n"


val _ = 
  UnitTest.test( "Testing component labeling",
    fn() =>
    let
      val T = true
      val F = false
      val image = 
        BooleanImage.fromList'( 5, 11, 
          [ T, T, T, F, F, F, F, F, F, F, F, 
            T, T, T, F, F, F, F, F, F, F, F,
            T, T, T, F, F, T, T, T, F, F, F,
            F, F, F, F, F, T, T, T, F, F, F,
            F, F, F, F, F, T, T, T, F, F, F ] )
    in
      ConnectedComponents.labelComponents image
    end , 
    fn x => 
    let
      val expected = 
        IntGrayscaleImage.fromList'( 5, 11, 
          [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
           1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
           1, 1, 1, 0, 0, 2, 2, 2, 0, 0, 0, 
           0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0, 
           0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0 ] ) 
    in
      IntGrayscaleImage.equal( x, expected )
    end )


