(* 
* file: test_sum_area_table.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the sum area table
*)


val _ = print"\n\n********** Sum area table tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="IntSumAreaTable", what="Building sum area table with integers",
    genInput=
      fn() =>
        [ ( 3, 
            3, 
            Array2.fromList[ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9  ] ] ) ] ,
    f= 
      fn[ i1 ] =>
        [ IntSumAreaTable.buildTable( 
            #1 i1, 
            #2 i1, 
            fn ( i, j ) => Array2.sub( #3 i1, i, j ) ) ] ,
    evaluate=
      fn[ o1 ] =>
      let
        val truth = Array2.fromList
                   [ [ 1,  3,  6  ],
                     [ 5,  12, 21 ],
                     [ 12, 27, 45 ] ]
        val range = { base=truth, row=0, col=0, nrows=NONE, ncols=NONE }
      in
        [ Array2.foldi Array2.RowMajor 
            ( fn ( i, j, p, a ) => a andalso p=Array2.sub( o1, i, j ) )
            true
            range ]
      end ,
    inputToString=
      fn( r, c, ar ) =>
        "( " ^ 
        Int.toString r ^ ", " ^
        Int.toString c ^ ", " ^
        Array2Util.toString Int.toString ar ^
        " )" }


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="IntSumAreaTable", what="Sum an area with integers",
    genInput=
      fn() =>
      let
        val im = 
          Array2.fromList[
            [ 1, 2, 3, 5 ],
            [ 6, 7, 8, 9 ],
            [ 10, 11, 12, 13 ],
            [ 14, 15, 16, 17 ] ]
        val table = IntSumAreaTable.buildTable
          ( 4, 4, fn ( i, j ) => Array2.sub( im, i, j ) )
      in
        [ ( table, ( 1, 1, 2, 2 ) ) ] 
      end ,
    f= fn[ i1 ] => [ IntSumAreaTable.sum ( #1 i1 ) ( #2 i1 ) ] ,
    evaluate= fn[ o1 ] => [ o1=38 ] ,
    inputToString=
      fn( t, ( i, j, r, c ) ) =>
        "( " ^
        Array2Util.toString Int.toString t ^ ", " ^
        "( " ^ 
        Int.toString i ^ ", " ^ 
        Int.toString j ^ ", " ^ 
        Int.toString r ^ ", " ^ 
        Int.toString c ^
        " ) )" }
          

