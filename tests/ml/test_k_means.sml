val _ = print"\n\n********** Testing KMeans **********\n"

val _ = UnitTest.test( "Testing KMeans.cluster",
  fn() => 
    [
      KMeans.cluster( 2, 1,
        [ [ 0.1 ], [ 0.2 ], [ 0.3 ], [ 0.6 ], [ 0.7 ], [ 0.8 ] ],
        100 ), 
      KMeans.cluster( 2, 2,
        [ [ 0.1, 0.9 ], [ 0.2, 0.2 ], [ 0.3, 0.1 ], [ 0.6, 0.6 ], [ 0.7, 0.2 ], [ 0.8, 0.7 ] ],
        100 )
    ],
  fn( [ ( A1, M1 ), ( A2, M2 ) ] ) => 
  let
    val _ = PrintUtil.printList Int.toString A1
    val _ = PrintUtil.printList ( ListUtil.toString Real.toString ) M1 
    val _ = print"\n"
    val _ = PrintUtil.printList Int.toString A2
    val _ = PrintUtil.printList ( ListUtil.toString Real.toString ) M2 
    val _ = print"\n"
  in
    true
  end )
