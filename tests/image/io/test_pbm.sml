(* 
* file: test_pbm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the PBM implementations.
*)

val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="BooleanPBM", what="read",
    genInput= fn() => [ "simple.plain.pbm", "simple.raw.pbm" ] ,
    f= 
      fn[ i1, i2 ] => [
        Option.valOf( BooleanPBM.read i1 ) ,
        Option.valOf( BooleanPBM.read i2 ) ] ,
    evaluate=
      fn[ o1, o2 ] =>
      let
        val truth = BooleanImage.fromList[ [ true, false ], [ true, false ] ]
      in [
        BooleanImage.equals( o1, truth ),
        BooleanImage.equals( o2, truth ) ]
      end ,
    inputToString= fn x => x }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="BooleanPBM", what="write",
    genInput= 
      fn() => [ 
        ( [ [ true, false ], [ true, false ] ], "output/write.plain.pbm" ), 
        ( [ [ true, false ], [ true, false ] ], "output/write.raw.pbm" ) ] , 
    f=
      fn[ i1, i2 ] =>
      let
        val im1 = BooleanImage.fromList ( #1 i1 )
        val im2 = BooleanImage.fromList ( #1 i2 )

        val _ = BooleanPBM.write( im1, #2 i1 )
        val _ = BooleanPBM.write' PNM.rawPBM ( im2, #2 i2 )
      in
        [ #2 i1, #2 i2 ]
      end ,
    evaluate=
      fn[ o1, o2 ] => 
      let
        val im1 = Option.valOf( BooleanPBM.read o1 )
        val im2 = Option.valOf( BooleanPBM.read o2 )

        val truth = 
          BooleanImage.fromList[ [ true, false ], [ true, false ] ]
      in [ 
        BooleanImage.equal( im1, truth ) andalso
        BooleanImage.equal( im2, truth ) ]
      end ,
    inputToString= 
      fn( xss, f )=> 
        "( " ^ 
        ListUtil.toString ListUtil.toString Bool.toString xss ^
        ", " ^ 
        f ^
        " )" } 

