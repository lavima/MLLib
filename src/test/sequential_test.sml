(*
* filename: sequential_test.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains sequential tests.
*)

signature SEQUENTIAL_TEST =
sig
  val test :  { group : string,
                what : string,
                num : int,
                genInput : int -> ('a * 'b),
                f : 'a -> 'b,
                compare : ('b * 'b) -> bool,
                inputToString : 'a -> string }
              -> 
              unit

  val test' : string list
              ->
              { group : string,
                what : string,
                num : int,
                genInput : int -> ('a * 'b),
                f : 'a -> 'b,
                compare : ('b * 'b) -> bool,
                inputToString : 'a -> string }
              -> 
              unit
end

structure SequentialTest : SEQUENTIAL_TEST =
struct

  open TestCommon

  fun test( { group : string,
              what : string,
              num : int,
              genInput : int -> ('a * 'b),
              f : 'a -> 'b,
              compare : ('b * 'b) -> bool,
              inputToString : 'a -> string } )
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
            ( spec : { 
                group : string,
                what : string,
                num : int,
                genInput : int -> ('a * 'b),
                f : 'a -> 'b,
                compare : ('b * 'b) -> bool,
                inputToString : 'a -> string } )
      : unit =
    case groups of
      [] => test spec
    | _ => 
      case isGroupMember( #group spec, groups ) of
        false => ()
      | true => test spec

end
