(* 
* file: test_filter_util.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the filter utilities
*)

val _ = print"\n\n********** FilterUtil tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FilterUtil", what="createGaussianMaskgPb",
    genInput= fn() => [ ( 0, ( 3.0, 9 ) ) ] ,
    f= fn[ i1 ] => [ FilterUtil.createGaussianMaskgPb ( #1 i1 ) ( #2, i1 ) ] ,
    evaluate= fn[ o1 ] => [ true ] ,
    inputToString= 
      fn( o, ( s, t ) ) =>
        "( " ^ 
        Int.toString o ^
        "( " ^ Real.toString s ^ ", " ^ Int.toString t ^ " )" ^
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FilterUtil", what="createGaussianMaskGPB2D",
    genInput= fn() => [ ( 2, ( 3.0, 3.0, 3.0, 1.0, false, 0.3 ) ) ] ,
    f= fn[ i1 ] => [ FilterUtil.createGaussianMaskGPB2D ( #1 i1 ) ( #2 i1 ) ] ,
    evaluate= fn[ o1 ] => [ true ] ,
    inputToString= 
      fn( o, ( s, sx, sy, e, h, ori ) ) =>
        "( " ^ 
        Int.toString o ^
          "( " ^ Real.toString s ^ ", " 
          ^ Int.toString t ^ " )" ^
        " )" }
  fn X =>
  let
    val norm = ImageUtil.normalizeReal'' X 
    val _ = RealPGM.write(norm, "output/output3dFilter.pgm");
  in
    true
  end )

val _ = SimpleTest.test( "Savgol filtering",
  fn() => 
  let
    val img = Option.valOf(RealPGM.read("proper.plain.pgm"))
  in
    FilterUtil.savgol(img, 3.0, 1.0, 0.6)
  end,
  fn x =>
  let
    val normalizedImage = ImageUtil.normalizeReal'' x
    val _ = RealPGM.write( normalizedImage, "output/savgolFiltering.pgm" )
  in
    true
  end )
