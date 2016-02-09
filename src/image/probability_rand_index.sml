
(*
* Generic function for calculating the Probability Rand index
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
             seg : 'a list, 
             truth : 'b list ) 
    : real = 
let
  val ( cooccur, width, height ) = 
    countCooccur( lessSeg, lessTruth, segToInt, truthToInt, seg, truth )

  val cooccurSize = width*height

  fun sumSquares( xs : int list ) : int = 
    List.foldl ( fn( x, y ) => x*x+y ) 0 xs

  fun sumCols( index : int, sum : int, sums : int list ) : int list =
    case index<cooccurSize of
      false => List.rev sums
    | true => 
        case index mod width=width-1 of
          false => sumCols( index+1, Array.sub( cooccur, index )+sum, sums )
        | true => 
            sumCols( index+1, 0, ( Array.sub( cooccur, index )+sum )::sums )

  fun sumRows( index : int, sum : int, sums : int list ) : int list =
    case index<cooccurSize of
      false => List.rev sums
    | true => 
        case ( index mod height )*width+( index div height ) of rindex =>
        case index mod height=height-1 of
          false => sumRows( index+1, Array.sub( cooccur, rindex )+sum, sums )
        | true => 
            sumRows( index+1, 0, ( Array.sub( cooccur, rindex )+sum )::sums )

  val n = Array.foldl ( fn( x, y ) => x+y ) 0 cooccur
  val nc2 = n*(n-1) div 2
  val nCols2 = sumSquares( sumCols( 0, 0, [] ) )
  val nRows2 = sumSquares( sumRows( 0, 0, [] ) )
  val n2 = Array.foldl ( fn( x, y ) => x*x+y ) 0 cooccur

in
  1.0 - ( ( real nCols2 )/2.0+( real nRows2 )/2.0-real n2 )/real nc2
end
