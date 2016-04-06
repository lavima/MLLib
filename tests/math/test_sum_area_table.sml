(* 
* file: test_sum_area_table.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the sum area table
*)


val _ = print"\n\n********** Sum area table tests **********\n"

val _ = UnitTest.test( "Building sum area table with integers",
  fn() =>
    let
      val im = Array2.fromList
                 [ [ 1, 2, 3 ],
                   [ 4, 5, 6 ],
                   [ 7, 8, 9  ] ]
    in
      IntSumAreaTable.buildTable( 3, 3, fn ( i, j ) => Array2.sub( im, i, j ) )
    end,
  fn x =>
    let
      val im = Array2.fromList
                 [ [ 1,  3,  6  ],
                   [ 5,  12, 21 ],
                   [ 12, 27, 45 ] ]
      val range = { base = im, row = 0, col = 0, nrows = NONE, ncols = NONE }
    in
      Array2.foldi Array2.RowMajor 
        ( fn ( i, j, p, a ) => 
          a andalso Int.compare( p, Array2.sub ( x, i, j ) ) = EQUAL )
        true
        range
    end )

val _ = UnitTest.test( "Sum an area with integers",
  fn() =>
    let
      val im = Array2.fromList
                 [ [ 1,   2,  3,  5 ],
                   [ 6,   7,  8,  9 ],
                   [ 10, 11, 12, 13 ],
                   [ 14, 15, 16, 17 ] ]
    
      val table = IntSumAreaTable.buildTable
        ( 4, 4, fn ( i, j ) => Array2.sub( im, i, j ) )

    in 
      IntSumAreaTable.sum table ( 1, 1, 2, 2 )
    end,
  fn x => Int.compare( x, 38 ) = EQUAL )
