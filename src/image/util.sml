
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
                  Seg1 : 'a list,
                  Seg2 : 'b list )
    : int Array.array * int * int =
let
  val NumSegs1 = seg1ToInt( Util.max lessSeg1 Seg1 )+1
  val NumSegs2 = seg2ToInt( Util.max lessSeg2 Seg2 )+1

  val Cooccur = Array.array( NumSegs1*NumSegs2, 0 )

  fun count( Ss1 : 'a list, Ss2 : 'b list ) : unit =
    case ( Ss1, Ss2 ) of
      ( [], [] ) => ()
    | ( S1::RSs1, S2::RSs2 ) => 
        case seg2ToInt S2*NumSegs1+seg1ToInt S1 of Index => (
          Array.update( 
            Cooccur, 
            Index, 
            Array.sub( Cooccur, Index )+1 );
          count( RSs1, RSs2 ) )

  val _ = count( Seg1, Seg2 )
in
  ( Cooccur, NumSegs1, NumSegs2 )
end

