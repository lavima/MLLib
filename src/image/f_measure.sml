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

  val Zero : score = ( 0, 0, 0, 0, 0.0, 0.0, 0.0 )

  fun add( S1 as ( CP1, SP1, CR1, SR1, P1, R1, F1 ) : score, 
           S2 as ( CP2, SP2, CR2, SR2, P2, R2, F2 ) : score ) 
      : score =
    ( CP1+CP2, SP1+SP2, CR1+CR2, SR1+SR2, P1+P2, R1+R2, F1+F2 )

  fun compare( S1 as ( _, _, _, _, _, _, F1 ) : score, 
            S2 as ( _, _, _, _, _, _, F2 ) : score ) : order =
    Real.compare( F1, F2 )

  fun toString( Score as ( CP, SP, CR, SR, P, R, F ) : score ) 
      : string =
    " Count Precision: " ^ Int.toString CP ^
    " Sum Precision: " ^ Int.toString SP ^
    " Count Recall: " ^ Int.toString CR ^
    " Sum Recall: " ^ Int.toString SR ^
    " Precision: " ^ Real.toString P ^
    " Recall: " ^ Real.toString R ^
    " F-measure: " ^ Real.toString F

end (* structure FMeasureCommon *)

(*
* This stucture is a wrapper around the Berkeley edge evaluator
*)
structure FMeasureBerkeleyEdge : SCORE =
struct

  open FMeasureCommon

  type image = BooleanImage.image
  type truth = BooleanImage.image
  
  val DefaultMaxDist = 0.0075
  val DefaultOutlierCost = 100.0;

  val matchEdges  = _import"fiMatchEdges" : 
    real Array.array * real Array.array * 
    int * int * real * real * 
    real Array.array * real Array.array -> real;

  fun evaluate( Image as { Width, Height, Values } : image, 
                Truths : truth list ) 
      : score =
  let
    
    val Diagonal = Math.sqrt( real( Width*Width + Height*Height ) )

    val MaxDist = DefaultMaxDist
    val OutlierCost = DefaultOutlierCost

    val ImageReal = ImageUtil.convertBooleanToTransposedReal Image

    val TruthReal = GrayscaleImageReal.image( Height, Width, 0.0 )
    val Match1 = GrayscaleImageReal.image( Height, Width, 0.0 )
    val Match2 = GrayscaleImageReal.image( Height, Width, 0.0 )


    val ( SumR, CountR, AccumMatch ) =
      List.foldl 
        ( fn( Truth, ( SumR, CountR, AccumMatch ) ) =>
          let
            val _ = ImageUtil.copyBooleanToTransposedReal( Truth, TruthReal )

            val Cost = 
              matchEdges( #Values ImageReal, #Values TruthReal, 
                          Width, Height, 
                          MaxDist*Diagonal, OutlierCost*MaxDist*Diagonal,
                          #Values Match1, #Values Match2 )

            val _ = 
              GrayscaleImageReal.appxy
                ( fn( X, Y, M ) => 
                    if M>0.0 then
                      BooleanImage.update( AccumMatch, X, Y, true )
                    else
                      () )
                Match1

            val CountR = 
              GrayscaleImageReal.foldl
                ( fn( M, Count ) => 
                    if M>0.0 then
                      Count+1
                    else
                      Count )
                CountR
                Match2

            val SumR = 
              BooleanImage.foldlxy
                ( fn( X, Y, M, Sum ) => 
                    if M then
                      Sum+1
                    else
                      Sum )
                SumR
                Truth

            val _ = GrayscaleImageReal.fill( Match1, 0.0 )
            val _ = GrayscaleImageReal.fill( Match2, 0.0 )

          in
            ( SumR, CountR, AccumMatch ) 
          end )
        ( 0, 0, BooleanImage.image( Height, Width, false ) )
        Truths
    
    val SumP = 
      BooleanImage.foldl
        ( fn( V, Sum ) => 
            if V then
              Sum+1 
            else
              Sum )
        0
        Image

    val CountP = 
      BooleanImage.foldl
        ( fn( V, Count ) => 
            if V then
              Count+1 
            else
              Count )
        0
        AccumMatch

    val P = real CountP/( case SumP>0 of false => 1.0 | true => real SumP )
    val R = real CountR/( case SumR>0 of false => 1.0 | true => real SumR )
    val F = 2.0*P*R/( case ( P+R )>0.0 of false => 1.0 | true => ( P+R ) )
  in
    ( CountP, SumP, CountR, SumR, P, R, F )
  end

  fun evaluateList( List : ( image * truth list ) list ) : score =
  let
    fun eval( List : ( image * truth list ) list, Accum : score ) : score =
      case List of
        [] => Accum
      | ( Image, Truths )::RList => 
          eval( RList, add( Accum, evaluate( Image, Truths ) ) )

    val ( CP, SP, CR, SR, _, _, _ ) = eval( List, Zero )

    val P = real CP/( case SP>0 of false => 1.0 | true => real SP )
    val R = real CR/( case SR>0 of false => 1.0 | true => real SR )
    val F = 2.0*P*R/( case ( P+R )>0.0 of false => 1.0 | true => ( P+R ) )
  in 
    ( CP, SP, CR, SR, P, R, F )
  end

  fun evaluateListAvg( List : ( image * truth list ) list ) : score =
  let
    fun eval( List : ( image * truth list ) list, Accum : score ) : score =
      case List of
        [] => Accum
      | ( Image, Truths )::RList => 
          eval( RList, add( Accum, evaluate( Image, Truths ) ) )
      
    val ( CP, SP, CR, SR, P, R, F ) = eval( List, Zero )
    val Length = List.length List
    val Length' = real Length
    val Score = 
      ( CP div Length, SP div Length, CR div Length, SR div Length, 
        P/Length', R/Length', F/Length' )
  in 
    Score
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

  val DefaultMaxDist = 0.0075
  val DefaultOutlierCost = 100.0;

  fun evaluate( Image as { Width, Height, Values } : image, 
                Truths : truth list ) 
      : score =
  let
    val [ Truth as { Values=TruthPixels, ... } ] = Truths
    val Diagonal = Math.sqrt( real Width*real Width + real Height*real Height )
    val MaxDistance = DefaultMaxDist*Diagonal
    val Degree = 6
    val OutlierCost = DefaultOutlierCost*MaxDistance
    val Multiplier = 100.0


    val M1 = Array2D.array( Width, Height, 0.0 )
    val M2 = Array2D.array( Width, Height, 0.0 )

    val Match1 = Array2D.array( Width, Height, ( ~1, ~1 ) )
    val Match2 = Array2D.array( Width, Height, ( ~1, ~1 ) )

    val R = Real.ceil DefaultMaxDist

    val Matchable1 = Array2D.array( Width, Height, false )
    val Matchable2 = Array2D.array( Width, Height, false )

    val MaxDistance2 = MaxDistance*MaxDistance

    val RToR = fromToInt( ~R, R )
    val _ = 
      BooleanImage.appxy
        ( fn( X1, Y1, Edge ) =>
            if Edge then
              List.app
                ( fn V => 
                    List.app
                      ( fn U => 
                        let
                          val D2 = real( U*U+V*V )
                          val X2 = X1+U
                          val Y2 = Y1+V
                        in
                          if ( D2>MaxDistance2 ) orelse
                             ( X2<0 orelse X2>=Width ) orelse
                             ( Y2<0 orelse Y2>=Height ) orelse
                             not ( BooleanImage.sub( Truth, X2, Y2 ) ) then
                            ()
                          else (
                            Array2D.update( Matchable1, Y1, X1, true );
                            Array2D.update( Matchable2, Y2, X2, true ) )
                        end )
                      RToR )
                RToR
            else
              () )
        Image

    val PixToNode1 = Array2D.array( Width, Height, ~1 )
    val PixToNode2 = Array2D.array( Width, Height, ~1 )
    val ( NumNodes1, NumNodes2, RevNodeToPix1, RevNodeToPix2 ) =
      BooleanImage.foldlxy
        ( fn( X, Y, _, ( Num1, Num2, Rev1, Rev2 ) ) => 
          let
            val ( Num1', Rev1' ) =
              if Array2D.sub( Matchable1, Y, X ) then (
                Array2D.update( PixToNode1, Y, X, Num1 );
                ( Num1+1, ( X, Y )::Rev1 ) )
              else
                ( Num1, Rev1 )

            val ( Num2', Rev2' ) =
              if Array2D.sub( Matchable2, Y, X ) then (
                Array2D.update( PixToNode2, Y, X, Num2 );
                ( Num2+1, ( X, Y )::Rev2 ) )
              else
                ( Num2, Rev2 )
          in
            ( Num1', Num2', Rev1', Rev2' )
          end )
        ( 0, 0, [], [] )
        Image

    val NodeToPix1 = Array.fromList( List.rev RevNodeToPix1 )
    val NodeToPix2 = Array.fromList( List.rev RevNodeToPix2 )

    val Edges = Array.fromList(
      BooleanImage.foldrxy
        ( fn( X1, Y1, Matchable, Edges ) =>
            if Matchable then
              List.foldr
                ( fn( V, Edges ) => 
                    List.foldr
                      ( fn( U, Edges ) => 
                        let
                          val D2 = real( U*U+V*V )
                          val X2 = X1+U
                          val Y2 = Y1+V
                        in
                          if ( D2>MaxDistance2 ) orelse
                             ( X2<0 orelse X2>=Width ) orelse
                             ( Y2<0 orelse Y2>=Height ) orelse
                             not ( Array2D.sub( Matchable2, Y2, X2 ) ) then
                            Edges 
                          else (
                            Array2D.sub( PixToNode1, Y1, X1 ),
                            Array2D.sub( PixToNode2, Y2, X2 ),
                            Math.sqrt D2 ) :: Edges
                        end )
                        Edges
                      RToR )
                Edges 
                RToR
            else
              Edges )
        []
        Image )

    val N = NumNodes1 + NumNodes2
    val NMin = minInt [ NumNodes1, NumNodes2 ]
    val NMax = maxInt [ NumNodes1, NumNodes2 ]
    
    val D1 = maxInt [ 0, minInt [ Degree, NumNodes1-1 ] ]
    val D2 = maxInt [ 0, minInt [ Degree, NumNodes2-1 ] ]
    val D3 = minInt [ Degree, NumNodes1, NumNodes2 ]
    val DMax = maxInt [ D1, D2, D3 ]

    val M = Array.length Edges + D1*NumNodes1 + D2*NumNodes2 + D3*NMax + N
  in
    if M=0 then
      ( 0.0, 0.0, 0.0 )
    else
      let
        val OW = Real.ceil( OutlierCost*Multiplier )
        val Outliers = Array.array( DMax, 0 )
        val IGraph = Array2D.array( M, 3, 0 )
        val Count = ref 0
        val inc = 
          fn Count => Count := ( !Count )+1

        val _ = List.app
          ( fn( I, J, W ) => ( 
              Array2D.update( IGraph, !Count, 0, I );
              Array2D.update( IGraph, !Count, 1, J );
              Array2D.update( IGraph, !Count, 2, 
                Real.toInt IEEEReal.TO_NEAREST W );
              inc Count
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

fun calcPRF( Seg : int list, Truth : int list ) 
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

  val Intersection = getIntersection( Seg, Truth )
  val IntersectionSize = real( List.length Intersection )
  val P = IntersectionSize/real( List.length Seg )
  val R = IntersectionSize/real( List.length Truth )

in
  ( P, R, P*R/( 0.5*( P+R ) ) )
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
                Seg : 'a list, 
                Truth : 'b list,
                TruthLabels : 'b list ) 
    : real * real * real =  
let
  
  val SegLabels : 'a list = unique( eq lessSeg, Seg )
  val SegLabelsArray : 'a Array.array = Array.fromList SegLabels
  val SegArray : int list Array.array = partition( eq lessSeg, Seg, SegLabels )

  val TruthLabelsArray : 'b Array.array = Array.fromList TruthLabels
  val TruthArray : int list Array.array = 
    partition( eq lessTruth, Truth, TruthLabels )

  fun loopSegments( Truth : int list, I : int ) 
    : ( real * real * real ) list =
    case I<Array.length SegLabelsArray of
      false => []
    | true => 
      let
        val ( P, R, F ) = calcPRF( Array.sub( SegArray, I ), Truth )
      in
        ( P, R, F )::loopSegments( Truth, I+1 )
      end

  fun loopTruths( I : int, Scores : ( real * real * real ) list ) 
    : real * real * real = 
    case I<Array.length TruthArray of
      false => 
      let
        val ( Num, TotalScore as ( TotalP, TotalR, TotalF ) ) =
          foldl( 
            ( fn( Score as ( P, R, F ), 
                  ( Count, TScore as ( TP, TR, TF ) ) ) =>
                ( Count+1.0, ( TP+P, TR+R, TF+F ) ) ), 
            ( 0.0, ( 0.0, 0.0, 0.0 ) ),
            Scores )
      in
        ( TotalP/Num, TotalR/Num, TotalF/Num )
      end        
    | true => 
      let
        val MaxScore = 
          max(  
            ( fn( Score1 as ( _, _, F1 ), Score2 as ( _, _, F2 ) ) => 
                if Real.isNan F1 then
                  true
                else if Real.isNan F2 then
                  false 
                else 
                  F1<F2 ),
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
              Seg : 'a list, 
              Truth : 'b list ) 
    : score =  
  calculate'( lessSeg, lessTruth, Seg, Truth, unique( eq lessTruth, Truth ) )
 
end (* struct *)

structure FMeasureForeground : SCORE =
struct

open FMeasureCommon

fun evaluate( lessSeg : 'a * 'a -> bool, 
              lessTruth : 'b * 'b -> bool, 
              Seg : 'a list, 
              Truth : 'b list ) 
    : score =  
let
  val ForegroundLabel = max( lessTruth, unique( eq lessTruth, Truth ) )
in
  calculate'( lessSeg, lessTruth, Seg, Truth, [ ForegroundLabel ] )
end

end (* struct *)

end (* local *)
*)
