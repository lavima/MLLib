(* 
* file: test_text_file_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the TextFileUtil structure
*)

val _ = print"\n\n********** TextFileUtil Tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="TextFileUtil", what="Testing TextFileUtil.writeDSV",
    genInput = fn() => [ [ ( 1, 2 ), ( 3, 4 ) ] ] ,
    f = 
      fn[ i1 ] => [
        TextFileUtil.writeDSV 
          ( "", 1 )
          ( fn( X, Y ) => Int.toString X ^ " " ^ Int.toString Y )
          ( i1, "output/output.dsv" ) ] ,
    evaluate = fn[ o1 ] => [ true ] ,
    inputToString = 
      ListUtil.toString 
        ( fn( x, y ) => "( " ^ Int.toString x ^ ", " ^ Int.toString y ^ " )" ) }
