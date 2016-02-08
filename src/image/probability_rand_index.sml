
(*
* Generic function for calculating the Probability Rand Index
* 
* The segmentation and the ground truth can be represented using different 
* types. The first two arguments are comparison functions used to compare the
* segmentation type and the ground truth type.
* 
* The function returns a single real representing the quality of the entire 
* segmentation.
*
* The implementation is a SML port of the PRI implemented in the BSDS500, which
* in turn was taken directly from an implementation by Allen Yang  
* (http://perception.csl.illinois.edu/coding/image_segmentation/)
*)
fun calcPRI( lessSeg : 'a * 'a -> bool, 
             lessTruth : 'b * 'b -> bool, 
             segToInt : 'a -> int,
             truthToInt : 'b -> int,
             Seg : 'a list, 
             Truth : 'b list ) 
    : real = 
let
  val ( Cooccur, Width, Height ) = 
    countCooccur( lessSeg, lessTruth, segToInt, truthToInt, Seg, Truth )

  val CooccurSize = Width*Height

  fun sumSquares( Xs : int list ) : int = 
    List.foldl ( fn( X, Y ) => X*X+Y ) 0 Xs

  fun sumCols( Index : int, Sum : int, Sums : int list ) : int list =
    case Index<CooccurSize of
      false => List.rev Sums
    | true => 
        case Index mod Width=Width-1 of
          false => sumCols( Index+1, Array.sub( Cooccur, Index )+Sum, Sums )
        | true => 
            sumCols( Index+1, 0, ( Array.sub( Cooccur, Index )+Sum )::Sums )

  fun sumRows( Index : int, Sum : int, Sums : int list ) : int list =
    case Index<CooccurSize of
      false => List.rev Sums
    | true => 
        case ( Index mod Height )*Width+( Index div Height ) of RIndex =>
        case Index mod Height=Height-1 of
          false => sumRows( Index+1, Array.sub( Cooccur, RIndex )+Sum, Sums )
        | true => 
            sumRows( Index+1, 0, ( Array.sub( Cooccur, RIndex )+Sum )::Sums )

  val N = Array.foldl ( fn( X, Y ) => X+Y ) 0 Cooccur
  val NC2 = N*(N-1) div 2
  val NCols2 = sumSquares( sumCols( 0, 0, [] ) )
  val NRows2 = sumSquares( sumRows( 0, 0, [] ) )
  val N2 = Array.foldl ( fn( X, Y ) => X*X+Y ) 0 Cooccur

in
  1.0 - ( ( real NCols2 )/2.0+( real NRows2 )/2.0-real N2 )/real NC2
end
