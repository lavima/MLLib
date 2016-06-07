signature SCORE =
sig
  
  type segMap
  type edgeMap
  type truth
  type score 

  val zeroScore : score
  
  val add : score * score -> score
  
  val evaluateSegmentation : segMap * truth list -> score
  val evaluateEdge : edgeMap * truth list -> score
  val evaluateSegmentationList : ( segMap * truth list ) list -> score
  val evaluateEdgeList : ( edgeMap * truth list ) list -> score
  val evaluateSegmentationListAvg : ( segMap * truth list ) list -> score
  val evaluateEdgeListAvg : ( edgeMap * truth list ) list -> score

  val toString : score -> string

end (* signature SCORE *)
