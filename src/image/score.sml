signature SCORE =
sig
  
  type image
  type truth
  type score 

  val Zero : score
  
  val add : score * score -> score
  
  val evaluate : image * truth list -> score
  val evaluateList : ( image * truth list ) list -> score
  val evaluateListAvg : ( image * truth list ) list -> score

  val toString : score -> string

end (* signature SCORE *)
