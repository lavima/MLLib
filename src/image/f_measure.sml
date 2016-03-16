(*
* filename: f_measure.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure for calculating the F-measure.
*)

structure FMeasureCommon =
struct

  exception fMeasureException of string

  type score = int * int * int * int * real * real * real

  val zeroScore : score = ( 0, 0, 0, 0, 0.0, 0.0, 0.0 )

  fun add( ( cp1, sp1, cr1, sr1, p1, r1, f1 ) : score, 
           ( cp2, sp2, cr2, sr2, p2, r2, f2 ) : score ) 
      : score =
    ( cp1+cp2, sp1+sp2, cr1+cr2, sr1+sr2, p1+p2, r1+r2, f1+f2 )

  fun compare( ( _, _, _, _, _, _, f1 ) : score, 
               ( _, _, _, _, _, _, f2 ) : score ) : order =
    Real.compare( f1, f2 )

  fun toString( ( cp, sp, cr, sr, p, r, f ) : score ) 
      : string =
    " Count Precision: " ^ Int.toString cp ^
    " Sum Precision: " ^ Int.toString sp ^
    " Count Recall: " ^ Int.toString cr ^
    " Sum Recall: " ^ Int.toString sr ^
    " Precision: " ^ Real.toString p ^
    " Recall: " ^ Real.toString r ^
    " F-measure: " ^ Real.toString f

end (* structure FMeasureCommon *)

(*
* This stucture is a wrapper around the Berkeley edge evaluator
*)
structure FMeasureBerkeleyEdge : SCORE =
struct

  open FMeasureCommon

  type image = BooleanImage.image
  type truth = BooleanImage.image
  
  val defaultMaxDist = 0.0075
  val defaultOutlierCost = 100.0;

  val matchEdges  = _import"fiMatchEdges" : 
    real Array.array * real Array.array * 
    int * int * real * real * 
    real Array.array * real Array.array -> real;

  fun evaluate( image : image, 
                truths : truth list ) 
      : score =
  let
    
    val ( height, width ) = BooleanImage.dimensions image
    
    val diagonal = Math.sqrt( real( width*width + height*height ) )

    val maxDist = defaultMaxDist
    val outlierCost = defaultOutlierCost

    val imageReal = Array.array( width*height, 0.0 )
    val _ = 
      BooleanImage.appi BooleanImage.ColMajor
        ( fn( i, j, x ) => 
            Array.update( imageReal, j*height+i, if x then 1.0 else 0.0 ) )
        ( BooleanImage.full image )
      
    val truthReal = Array.array( width*height, 0.0 )

    val match1 = Array.array( width*height, 0.0 )
    val match2 = Array.array( width*height, 0.0 )

    val ( sumR, countR, accumMatch ) =
      List.foldl 
        ( fn( truth, ( sumR, countR, accumMatch ) ) =>
          let
            val _ = 
              BooleanImage.appi BooleanImage.ColMajor
                ( fn( i, j, x ) => 
                    Array.update( 
                      truthReal, 
                      j*height+i, 
                      if x then 1.0 else 0.0 ) )
              ( BooleanImage.full truth )

            val cost = 
              matchEdges( imageReal, truthReal, 
                          width, height, 
                          maxDist*diagonal, outlierCost*maxDist*diagonal,
                          match1, match2 )

            val _ = 
              Array.appi
                ( fn( i, x ) => 
                    if x>0.0 then
                      Array.update( accumMatch, i, true )
                    else
                      () )
                match1

            val countR = 
              Array.foldl
                ( fn( m, count ) => 
                    if m>0.0 then
                      count+1
                    else
                      count )
                countR
                match2

            val sumR = 
              BooleanImage.fold BooleanImage.RowMajor
                ( fn( m, sum ) => 
                    if m then
                      sum+1
                    else
                      sum )
                sumR
                truth

            val _ = ArrayUtil.fill( match1, 0.0 )
            val _ = ArrayUtil.fill( match2, 0.0 )

          in
            ( sumR, countR, accumMatch ) 
          end )
        ( 0, 0, Array.array( width*height, false ) )
        truths
    
    val sumP = 
      BooleanImage.fold BooleanImage.RowMajor
        ( fn( v, sum ) => 
            if v then
              sum+1 
            else
              sum )
        0
        image

    val countP = 
      Array.foldl
        ( fn( v, count ) => 
            if v then
              count+1 
            else
              count )
        0
        accumMatch

    val p = real countP/( case sumP>0 of false => 1.0 | true => real sumP )
    val r = real countR/( case sumR>0 of false => 1.0 | true => real sumR )
    val f = 2.0*p*r/( case ( p+r )>0.0 of false => 1.0 | true => ( p+r ) )
  in
    ( countP, sumP, countR, sumR, p, r, f )
  end

  fun evaluateList( evalList : ( image * truth list ) list ) : score =
  let
    fun eval( evalList : ( image * truth list ) list, accum : score ) : score =
      case evalList of
        [] => accum
      | ( image, truths )::evalList' => 
          eval( evalList', add( accum, evaluate( image, truths ) ) )

    val ( cp, sp, cr, sr, _, _, _ ) = eval( evalList, zeroScore )

    val p = real cp/( case sp>0 of false => 1.0 | true => real sp )
    val r = real cr/( case sr>0 of false => 1.0 | true => real sr )
    val f = 2.0*p*r/( case ( p+r )>0.0 of false => 1.0 | true => ( p+r ) )
  in 
    ( cp, sp, cr, sr, p, r, f )
  end

  fun evaluateListAvg( evalList : ( image * truth list ) list ) : score =
  let
    fun eval( evalList : ( image * truth list ) list, accum : score ) : score =
      case evalList of
        [] => accum
      | ( image, truths )::evalList' => 
          eval( evalList', add( accum, evaluate( image, truths ) ) )
      
    val ( cp, sp, cr, sr, p, r, f ) = eval( evalList, zeroScore )
    val length = List.length evalList
    val length' = real length
    val score = 
      ( cp div length, sp div length, cr div length, sr div length, 
        p/length', r/length', f/length' )
  in 
    score
  end

end (* structure FMeasureBerkeleyEdge *)

(* 
* Incomplete SML implementation of the Berkeley edge evaluator 
*)
(*
structure FMeasureEdge : SCORE = 
struct

  open FMeasureCommon
  
  type image = BooleanImage.image
  type truth = BooleanImage.image

  val defaultMaxDist = 0.0075
  val defaultOutlierCost = 100.0;

  fun evaluate( image as { width, height, values } : image, 
                truths : truth list ) 
      : score =
  let
    val [ truth as { values=TruthPixels, ... } ] = truths
    val diagonal = Math.sqrt( real width*real width + real height*real height )
    val MaxDistance = defaultMaxDist*diagonal
    val Degree = 6
    val outlierCost = defaultOutlierCost*MaxDistance
    val Multiplier = 100.0


    val M1 = Array2D.array( width, height, 0.0 )
    val M2 = Array2D.array( width, height, 0.0 )

    val match1 = Array2D.array( width, height, ( ~1, ~1 ) )
    val match2 = Array2D.array( width, height, ( ~1, ~1 ) )

    val r = Real.ceil defaultMaxDist

    val Matchable1 = Array2D.array( width, height, false )
    val matchable2 = Array2D.array( width, height, false )

    val MaxDistance2 = MaxDistance*MaxDistance

    val RToR = fromToInt( ~r, r )
    val _ = 
      BooleanImage.appxy
        ( fn( X1, Y1, Edge ) =>
            if Edge then
              List.app
                ( fn v => 
                    List.app
                      ( fn U => 
                        let
                          val D2 = real( U*U+v*v )
                          val X2 = X1+U
                          val Y2 = Y1+v
                        in
                          if ( D2>MaxDistance2 ) orelse
                             ( X2<0 orelse X2>=width ) orelse
                             ( Y2<0 orelse Y2>=height ) orelse
                             not ( BooleanImage.sub( truth, X2, Y2 ) ) then
                            ()
                          else (
                            Array2D.update( Matchable1, Y1, X1, true );
                            Array2D.update( matchable2, Y2, X2, true ) )
                        end )
                      RToR )
                RToR
            else
              () )
        image

    val pixToNode1 = Array2D.array( width, height, ~1 )
    val pixToNode2 = Array2D.array( width, height, ~1 )
    val ( NumNodes1, NumNodes2, RevNodeToPix1, RevNodeToPix2 ) =
      BooleanImage.foldlxy
        ( fn( x, y, _, ( Num1, Num2, Rev1, Rev2 ) ) => 
          let
            val ( Num1', Rev1' ) =
              if Array2D.sub( Matchable1, y, x ) then (
                Array2D.update( pixToNode1, y, x, Num1 );
                ( Num1+1, ( x, y )::Rev1 ) )
              else
                ( Num1, Rev1 )

            val ( Num2', Rev2' ) =
              if Array2D.sub( matchable2, y, x ) then (
                Array2D.update( pixToNode2, y, x, Num2 );
                ( Num2+1, ( x, y )::Rev2 ) )
              else
                ( Num2, Rev2 )
          in
            ( Num1', Num2', Rev1', Rev2' )
          end )
        ( 0, 0, [], [] )
        image

    val NodeToPix1 = Array.fromList( List.rev RevNodeToPix1 )
    val NodeToPix2 = Array.fromList( List.rev RevNodeToPix2 )

    val Edges = Array.fromList(
      BooleanImage.foldrxy
        ( fn( X1, Y1, Matchable, Edges ) =>
            if Matchable then
              List.foldr
                ( fn( v, Edges ) => 
                    List.foldr
                      ( fn( U, Edges ) => 
                        let
                          val D2 = real( U*U+v*v )
                          val X2 = X1+U
                          val Y2 = Y1+v
                        in
                          if ( D2>MaxDistance2 ) orelse
                             ( X2<0 orelse X2>=width ) orelse
                             ( Y2<0 orelse Y2>=height ) orelse
                             not ( Array2D.sub( matchable2, Y2, X2 ) ) then
                            Edges 
                          else (
                            Array2D.sub( pixToNode1, Y1, X1 ),
                            Array2D.sub( pixToNode2, Y2, X2 ),
                            Math.sqrt D2 ) :: Edges
                        end )
                        Edges
                      RToR )
                Edges 
                RToR
            else
              Edges )
        []
        image )

    val N = NumNodes1 + NumNodes2
    val NMin = minInt [ NumNodes1, NumNodes2 ]
    val NMax = maxInt [ NumNodes1, NumNodes2 ]
    
    val D1 = maxInt [ 0, minInt [ Degree, NumNodes1-1 ] ]
    val D2 = maxInt [ 0, minInt [ Degree, NumNodes2-1 ] ]
    val D3 = minInt [ Degree, NumNodes1, NumNodes2 ]
    val DMax = maxInt [ D1, D2, D3 ]

    val m = Array.length Edges + D1*NumNodes1 + D2*NumNodes2 + D3*NMax + N
  in
    if m=0 then
      ( 0.0, 0.0, 0.0 )
    else
      let
        val OW = Real.ceil( outlierCost*Multiplier )
        val Outliers = Array.array( DMax, 0 )
        val IGraph = Array2D.array( m, 3, 0 )
        val count = ref 0
        val inc = 
          fn count => count := ( !count )+1

        val _ = List.app
          ( fn( I, J, W ) => ( 
              Array2D.update( IGraph, !count, 0, I );
              Array2D.update( IGraph, !count, 1, J );
              Array2D.update( IGraph, !count, 2, 
                Real.toInt IEEEReal.TO_NEAREST W );
              inc count
              ) )
            
      in
        ( 0.0, 0.0, 0.0 )  
      end

  end 

end (* structure FMeasureEdge *)
*)


(*
local

fun partition( eq : 'a * 'a -> bool, LabeledPixels : 'a list, Labels : 'a list )
  : ( int list ) Array.array =
let
  val SegmentArray = Array.array( List.length Labels, [] )

  fun getPixels( Pixels : 'a list, Label : 'a, Index : int ) : int list =
    case Pixels of
      [] => []
    | Pixel::RPixels => 
        case eq( Pixel, Label ) of 
          true => Index::getPixels( RPixels, Label, Index+1 )
        | false => getPixels( RPixels, Label, Index+1 )

  fun fill( Ls : 'a list, Index : int ) : unit =
    case Ls of
      [] => ()
    | L::RLs => ( 
        Array.update( SegmentArray, Index, 
          getPixels( LabeledPixels, L, 0 ) );
        fill( RLs, Index+1 ) )

  val _ = fill( Labels, 0 )
in
  SegmentArray
end

fun calcPRF( seg : int list, truth : int list ) 
  : real * real * real =
let
  fun getIntersection( Seg1 : int list, Seg2 : int list ) : int list =
    case ( Seg1, Seg2 ) of 
      ( I1::RSeg1, I2::RSeg2 ) => 
          if I1<I2 then 
            getIntersection( RSeg1, Seg2 ) 
          else if I2<I1 then 
            getIntersection( Seg1, RSeg2 )
          else 
            I1::getIntersection( RSeg1, RSeg2 )
    | ( _, _ ) => []

  val Intersection = getIntersection( seg, truth )
  val IntersectionSize = real( List.length Intersection )
  val p = IntersectionSize/real( List.length seg )
  val r = IntersectionSize/real( List.length truth )

in
  ( p, r, p*r/( 0.5*( p+r ) ) )
end

(*
* Generic function for calculating the F-measure of a segmentation.
* 
* The segmentation and the ground truth can be represented using different 
* types. The first two arguments are comparison functions used to compare the
* segmentation type and the ground truth type.
* 
* TruthLabels specify the list of ground truth labels that are of interest.
* 
* The function returns a tuple with the average precision, recall and f-measure
* of the best matching segments.
*
* The implementation is a SML port of the f-measure implementation in the 
* Weizmann Segmentation Evalution Database 
* (http://www.wisdom.weizmann.ac.il/~vision/Seg_Evaluation_DB/)
*)

fun calculate'( lessSeg : 'a * 'a -> bool, 
                lessTruth : 'b * 'b -> bool, 
                seg : 'a list, 
                truth : 'b list,
                TruthLabels : 'b list ) 
    : real * real * real =  
let
  
  val SegLabels : 'a list = unique( eq lessSeg, seg )
  val SegLabelsArray : 'a Array.array = Array.fromList SegLabels
  val SegArray : int list Array.array = partition( eq lessSeg, seg, SegLabels )

  val TruthLabelsArray : 'b Array.array = Array.fromList TruthLabels
  val TruthArray : int list Array.array = 
    partition( eq lessTruth, truth, TruthLabels )

  fun loopSegments( truth : int list, I : int ) 
    : ( real * real * real ) list =
    case I<Array.length SegLabelsArray of
      false => []
    | true => 
      let
        val ( p, r, f ) = calcPRF( Array.sub( SegArray, I ), truth )
      in
        ( p, r, f )::loopSegments( truth, I+1 )
      end

  fun loopTruths( I : int, Scores : ( real * real * real ) list ) 
    : real * real * real = 
    case I<Array.length TruthArray of
      false => 
      let
        val ( Num, TotalScore as ( TotalP, TotalR, TotalF ) ) =
          foldl( 
            ( fn( score as ( p, r, f ), 
                  ( count, TScore as ( TP, TR, TF ) ) ) =>
                ( count+1.0, ( TP+p, TR+r, TF+f ) ) ), 
            ( 0.0, ( 0.0, 0.0, 0.0 ) ),
            Scores )
      in
        ( TotalP/Num, TotalR/Num, TotalF/Num )
      end        
    | true => 
      let
        val MaxScore = 
          max(  
            ( fn( Score1 as ( _, _, f1 ), Score2 as ( _, _, f2 ) ) => 
                if Real.isNan f1 then
                  true
                else if Real.isNan f2 then
                  false 
                else 
                  f1<f2 ),
            loopSegments( Array.sub( TruthArray, I ), 0 ) )
      in
        loopTruths( I+1, MaxScore::Scores )
      end        

in
  loopTruths( 0, [] ) 
end

in

structure FMeasureAverage : SCORE =
struct

open FMeasureCommon

fun evaluate( lessSeg : 'a * 'a -> bool, 
              lessTruth : 'b * 'b -> bool, 
              seg : 'a list, 
              truth : 'b list ) 
    : score =  
  calculate'( lessSeg, lessTruth, seg, truth, unique( eq lessTruth, truth ) )
 
end (* struct *)

structure FMeasureForeground : SCORE =
struct

open FMeasureCommon

fun evaluate( lessSeg : 'a * 'a -> bool, 
              lessTruth : 'b * 'b -> bool, 
              seg : 'a list, 
              truth : 'b list ) 
    : score =  
let
  val ForegroundLabel = max( lessTruth, unique( eq lessTruth, truth ) )
in
  calculate'( lessSeg, lessTruth, seg, truth, [ ForegroundLabel ] )
end

end (* struct *)

end (* local *)
*)
