(* 
* file: test_image.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the FMeasure implementation.
*)


val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FMeasureBerkeley", what="evaluateEdge",
    genInput= 
      fn() => [
        ( BooleanImage.fromList[ 
            [ true, false, false ], 
            [ false, true, false ], 
            [ false, false, true ] ],
          [ BooleanImage.fromList[
              [ true, false, false ], 
              [ false, true, false ], 
              [ false, true, false ] ] ] ),
        ( Option.valOf( BooleanPBM.read( "edge_classified.plain.pbm" ) ),
          List.map
            ( fn Filename => 
                Option.valOf( BooleanPBM.read Filename ) )
            [ "edge_truth_1.plain.pbm",
              "edge_truth_2.plain.pbm",
              "edge_truth_3.plain.pbm",
              "edge_truth_4.plain.pbm",
              "edge_truth_5.plain.pbm",
              "edge_truth_6.plain.pbm" ] ) ] ,
    f=
      fn[ i1, i2 ] =>
        [ FMeasureBerkeley.evaluateEdge( #1 i1, #2 i1 ),
          FMeasureBerkeley.evaluateEdge( #1 i2, #2 i2 ) ] ,
    evaluate=
      fn[ score1 as ( _, _, _, _, _, _, f1 ), 
          score2 as ( _, _, _, _, _, _, f2 ),
          score3 as ( _, _, _, _, _, _, f3 ) ] => 
        [ Util.approxEqReal( F1, 0.666, 3 ),
          Util.approxEqReal( F2, 0.495956270745496, 2 )
        (* P = 0.352722029988466
           R = 0.835059185285489
           F = 0.495956270745496 *) ] ,
    inputToString= 
      fn( i, ts ) =>
        "( " ^ 
        GrayscaleImageReal.toString i ^ ", " ^
        ListUtil.toString BooleanImage.toString ts ^
        " )" }
          

val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FMeasureBerkeley", what="evaluateSegmentation",
    genInput=
      fn() => [
        ( Option.valOf( IntPGM.read( "proper2.seg.raw.pgm" ) ),
          List.map
            ( fn Filename => 
                Option.valOf( BooleanPBM.read Filename ) )
            [ "edge_truth_1.plain.pbm",
              "edge_truth_2.plain.pbm",
              "edge_truth_3.plain.pbm",
              "edge_truth_4.plain.pbm",
              "edge_truth_5.plain.pbm",
              "edge_truth_6.plain.pbm" ] ) ] ,
    f=
      fn[ i1 ] => [ FMeasureBerkeley.evaluateSegmentation( #1 i1, #2 i1 ) ] ,
    evaluate=
      fn[ o1 as ( _, _, _, _, _, _, f ) ] =>
        [ Util.approxEqReal( F3, 0.767055072099290, 2 ) 
        (* P = 0.914462944825335
           R = 0.660573234032950
           F = 0.767055072099290
        *) ] ,
    inputToString= 
      fn( i, ts ) =>
        "( " ^ 
        GrayscaleImageReal.toString i ^ ", " ^
        ListUtil.toString BooleanImage.toString ts ^
        " )" }

