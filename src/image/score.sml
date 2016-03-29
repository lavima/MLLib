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
  val evaluateList : ( edgeMap * truth list ) list -> score
  val evaluateListAvg : ( edgeMap * truth list ) list -> score

  val toString : score -> string

end (* signature SCORE *)
