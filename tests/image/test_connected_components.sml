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
        BooleanImage.fromList( 11, 5, 
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
        GrayscaleImageInt.fromList( 11, 5, 
          [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
           1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
           1, 1, 1, 0, 0, 2, 2, 2, 0, 0, 0, 
           0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0, 
           0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0 ] ) 
    in
      GrayscaleImageInt.equal( x, expected )
    end )


