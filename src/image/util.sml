
(*
* Generic function for counting the co-occurrance between two segmentations
*
* The function returns an array representing the co-occurrance matrix along with
* the number of regions in both the segmentation and the ground truth---which 
* corresponds to the dimensionality of the matrix. The number of columns in the
* matrix is the number of regions in first segmentation and the number of rows 
* is the number of regions in the second segmentation.
*)
fun countCooccur( lessSeg1 : 'a * 'a -> bool,
                  lessSeg2 : 'b * 'b -> bool, 
                  seg1ToInt : 'a -> int,
                  seg2ToInt : 'b -> int,
                  seg1 : 'a list,
                  seg2 : 'b list )
    : int Array.array * int * int =
let
  val numSegs1 = seg1ToInt( Util.max lessSeg1 seg1 )+1
  val numSegs2 = seg2ToInt( Util.max lessSeg2 seg2 )+1

  val cooccur = Array.array( numSegs1*numSegs2, 0 )

  fun count( ss1 : 'a list, ss2 : 'b list ) : unit =
    case ( ss1, ss2 ) of
      ( [], [] ) => ()
    | ( s1::ss1', s2::ss2' ) => 
        case seg2ToInt s2*numSegs1+seg1ToInt s1 of index => (
          Array.update( 
            cooccur, 
            index, 
            Array.sub( cooccur, index )+1 );
          count( ss1', ss2' ) )

  val _ = count( seg1, seg2 )
in
  ( cooccur, numSegs1, numSegs2 )
end

