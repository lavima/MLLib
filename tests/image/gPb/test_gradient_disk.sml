(* 
* file: test_gradient_disk.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the gradient using disks
*)

val _ = print"\n\n********** Gradient tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Gradient disk", what="buildOrientationSliceMap",
    genInput=
      fn() => 
        [ ( 7, 7, 8 ) ] ,
    f= fn[ i1 ] => [ 
      buildOrientationSliceMap i1 ] ,
    evaluate= 
      fn[ o1 ] => 
      let
        val expected = IntGrayscaleImage.fromList  
          [ [ 2,  1,  1,  0, 15, 14, 14 ], 
            [ 2,  2,  1,  0, 15, 14, 14 ],  
            [ 2,  2,  2,  0, 15, 14, 13 ],  
            [ 3,  3,  3,  2, 14, 12, 12 ],  
            [ 4,  4,  4,  6, 10, 11, 11 ],  
            [ 5,  5,  6,  7,  8, 10, 10 ],  
            [ 5,  6,  6,  7,  8,  9, 10 ] ]
        val _ = print( IntGrayscaleImage.toString o1 ) 
      in
        [ IntGrayscaleImage.equal( o1, expected ) ]
      end ,
    inputToString=
      fn( x, y, z ) =>
        "( " ^ 
        Int.toString x ^ ", " ^ 
        Int.toString y ^ ", " ^
        Int.toString z ^ 
        " )" }
