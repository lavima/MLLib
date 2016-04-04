(*
* filename: sequential_test.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains sequential tests.
*)

structure SequentialTest : TEST =
struct

  open TestCommon

  type ( 'a, 'b )testSpec = {
    group : string,
    what : string,
    num : int,
    genInput : int -> ('a * 'b),
    f : 'a -> 'b,
    compare : ('b * 'b) -> bool,
    inputToString : 'a -> string }


  fun test( { group : string,
              what : string,
              num : int,
              genInput : int -> ('a * 'b),
              f : 'a -> 'b,
              compare : ('b * 'b) -> bool,
              inputToString : 'a -> string } : ( 'a, 'b )testSpec )
      : unit = 
  let
    
    val resultFile = getResultLog( group, what )

    fun iterateTests( remaining : int ) : bool =
    let
      val ( args, expected ) = genInput( num-remaining )
   
      val result = f args
      val success = compare( result, expected )

      val _ = addResult( resultFile, inputToString args, success )
    in
      if remaining > 0 then 
        success andalso iterateTests(remaining - 1)
      else 
        success
    end

    val success = iterateTests num

    val _ = print( group ^ ": " ^ what ^ " : " ^ resultToString success )
  in
    ()
  end

  fun test' ( groups : string list )
            ( spec : ( 'a, 'b )testSpec )
      : unit =
    case groups of
      [] => test spec
    | _ => 
      case isGroupMember( #group spec, groups ) of
        false => ()
      | true => test spec

end
