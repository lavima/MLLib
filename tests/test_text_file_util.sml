(* 
* file: test_text_file_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the TextFileUtil structure
*)

val _ = print"\n\n********** TextFileUtil Tests **********\n"

val _ = test( "Testing TextFileUtil.writeDSV",
  fn() =>
    TextFileUtil.writeDSV 
      ( "", 1 )
      ( fn( X, Y ) => Int.toString X ^ " " ^ Int.toString Y )
      ( [ ( 1, 2 ), ( 3, 4 ) ], "output.dsv" ) ,
  fn _ => true )
