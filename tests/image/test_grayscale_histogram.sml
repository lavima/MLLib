(* 
* file: test_grayscale_histogram.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the grayscale histogram library 
* implementation.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RealGrayscaleHistogram", what="histogram",
    genInput= 
      fn() => [
        ( 4, 
          RealGrayscaleImage.fromList'( 3, 3, 
          [ 1.0, 0.5, 0.3, 0.54, 0.0, 0.1, 0.65, 0.56, 0.31 ] ) ),
        ( 64, Option.valOf( RealPGM.read "proper.plain.pgm" ) ) ] ,
    f= 
      fn[ i1, i2 ] => [ 
        RealGrayscaleHistogram.histogram' ( #1 i1 ) ( #2 i1 ),
        RealGrayscaleHistogram.histogram' ( #1 i2 ) ( #2 i2 ) ] ,
    evaluate=
      fn[ o1, o2 ] => 
      let
        val truth1 = Array.fromList( [ 2, 2, 4, 1 ] )
        val truth2 = 
          Array.fromList( 
            [ 2247, 527, 335, 181, 98, 71, 47, 34, 
              34, 70, 191, 138, 168, 258, 406, 579, 
              1022, 22820, 39792, 18471, 3054, 727, 819, 1068, 
              1218, 1567, 1992, 2179, 2368, 2539, 2588, 2532, 
              2401, 2237, 2067, 1905, 1645, 1491, 1440, 1257, 
              1067, 833, 876, 617, 510, 475, 392, 378,
              439, 430, 427, 391, 417, 459, 442, 492, 
              509, 532, 497, 389, 115, 30, 0, 0 ] ) 
      in [
        Array.foldli 
          ( fn( i, x, t ) => t andalso x=Array.sub( truth1, i ) )
          true
          o1 ,
        Array.foldli 
          ( fn( i, x, t ) => t andalso x=Array.sub( truth2, i ) )
          true
          o2 ]
      end ,
    inputToString= 
      fn( n, im ) => 
        "( " ^ 
        Int.toString n ^ ", " ^
        RealGrayscaleImage.toString im ^ 
        " )" }

