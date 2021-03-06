(* 
* file: test_array_sort.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the functionality in the ArraySort 
* structure.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ArraySort", what="insertion",
    genInput = 
      fn() => [ 
        Array.fromList[ 0, 1, 2 ],
        Array.fromList[ 2, 1, 0 ],
        Array.fromList[ 0, 0, 0 ],
        Array.fromList[ 3, 5, 2, 1, 6, 7, 0, 8, 4, 9 ],
        Array.fromList[ 0, 1, 2, 1, 1, 2, 0, 1, 2, 2 ] ],
    f = 
      fn[ i1, i2, i3, i4, i5 ] => [ 
        ArraySort.insertion Int.< i1,
        ArraySort.insertion Int.< i2,
        ArraySort.insertion Int.< i3,
        ArraySort.insertion Int.< i4,
        ArraySort.insertion Int.< i5 ] , 
    evaluate = 
      fn[ o1, o2, o3, o4, o5 ] =>
      let
        val t1 = Array.fromList[ 0, 1, 2 ]
        val t2 = Array.fromList[ 0, 1, 2 ]
        val t3 = Array.fromList[ 0, 0, 0 ]
        val t4 = Array.fromList( ListUtil.fromToInt( 0, 9 ) )
        val t5 = Array.fromList[ 0, 0, 1, 1, 1, 1, 2, 2, 2, 2 ]
      in
        [ ArrayUtil.allEq Util.eqInt ( o1, t1 ),
          ArrayUtil.allEq Util.eqInt ( o2, t2 ), 
          ArrayUtil.allEq Util.eqInt ( o3, t3 ), 
          ArrayUtil.allEq Util.eqInt ( o4, t4 ),
          ArrayUtil.allEq Util.eqInt ( o5, t5 ) ] 
      end ,
    inputToString = ArrayUtil.toString Int.toString }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ArraySort", what="quick",
    genInput = 
      fn() => [ 
        Array.fromList[ 0, 1, 2 ],
        Array.fromList[ 2, 1, 0 ],
        Array.fromList[ 0, 0, 0 ],
        Array.fromList[ 3, 5, 2, 1, 6, 7, 0, 8, 4, 9 ],
        Array.fromList[ 0, 1, 2, 1, 1, 2, 0, 1, 2, 2 ] ],
    f = 
      fn[ i1, i2, i3, i4, i5 ] => [ 
        ArraySort.quick Int.< i1,
        ArraySort.quick Int.< i2,
        ArraySort.quick Int.< i3,
        ArraySort.quick Int.< i4,
        ArraySort.quick Int.< i5 ] , 
    evaluate = 
      fn[ o1, o2, o3, o4, o5 ] =>
      let
        val t1 = Array.fromList[ 0, 1, 2 ]
        val t2 = Array.fromList[ 0, 1, 2 ]
        val t3 = Array.fromList[ 0, 0, 0 ]
        val t4 = Array.fromList( ListUtil.fromToInt( 0, 9 ) )
        val t5 = Array.fromList[ 0, 0, 1, 1, 1, 1, 2, 2, 2, 2 ]
      in
        [ ArrayUtil.allEq Util.eqInt ( o1, t1 ),
          ArrayUtil.allEq Util.eqInt ( o2, t2 ), 
          ArrayUtil.allEq Util.eqInt ( o3, t3 ), 
          ArrayUtil.allEq Util.eqInt ( o4, t4 ),
          ArrayUtil.allEq Util.eqInt ( o5, t5 ) ] 
      end ,
    inputToString = ArrayUtil.toString Int.toString }

