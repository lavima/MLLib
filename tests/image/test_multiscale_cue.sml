(* 
* file: test_texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests for validating texton generation
*)

val _ = print"\n\n********** Multiscale cue tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="MultiscaleCue", what="orientedMultiscale",
    genInput=
      fn() =>
        [ ( Option.valOf( RealPGM.read "proper.plain.pgm" ),
            [ 1.0, 1.0, 1.0 ],
            32,
            10,
            Math.pi/4.0, 
            [ ( 3.0, 3.0/4.0 ), ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ) ] ) ] ,
    f= fn[ i1 ] => [ MultiscaleCue.orientedMultiscale i1 ] ,
    evaluate= fn[ o1 ] => [ true ] ,
    inputToString=
      fn( im, ws, b, s, ori, savgol ) =>
        "( " ^ 
        RealGrayscaleImage.toString im ^ ", " ^
        ListUtil.toString Real.toString ws ^ ", " ^ 
        Int.toString b ^ ", " ^
        Int.toString s ^ ", " ^
        Real.toString ori ^ ", " ^
        ListUtil.toString 
          ( fn( x, y ) => 
              "( " ^ Real.toString x ^ ", " ^ Real.toString y ^ " )" )
          savgol ^ 
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="MultiscaleCue", what="multiscale",
    genInput=
      fn() =>
      let
        val im = Option.valOf( RealPGM.read "proper.plain.pgm" )
      in
        [ ( im,
            im,
            [ 1.0, 1.0, 1.0 ],
            32,
            10,
            8 ) ]
      end , 
    f= fn[ i1 ] => [ MultiscaleCue.multiscale i1 ] ,
    evaluate= fn[ o1 ] => [ true ] ,
    inputToString=
      fn( im1, im2, ws, b, s, ori ) =>
        "( " ^ 
        RealGrayscaleImage.toString im1 ^ ", " ^
        RealGrayscaleImage.toString im2 ^ ", " ^
        ListUtil.toString Real.toString ws ^ ", " ^ 
        Int.toString b ^ ", " ^
        Int.toString s ^ ", " ^
        Int.toString ori ^ 
        " )" }
