(*
* filename: differential_test.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains a structure to do differential tests.
*)

signature DIFFERENTIAL_TEST =
sig
  val test :  { group : string,
                what : string,
                num : int,
                genInput : int -> 'a,
                fs : ('a -> 'b) list,
                compare : ('b * 'b) -> bool,
                inputToString : 'a -> string }
              ->
              unit

  val test' : string list 
              ->
              { group : string,
                what : string,
                num : int,
                genInput : int -> 'a,
                fs : ('a -> 'b) list,
                compare : ('b * 'b) -> bool,
                inputToString : 'a -> string }
              ->
              unit
end

structure DifferentialTest : DIFFERENTIAL_TEST =
struct

  open TestCommon

  fun test( { group : string,
              what : string,
              num : int,
              genInput : int -> 'a,
              fs : ('a -> 'b) list,
              compare : ('b * 'b) -> bool,
              inputToString : 'a -> string } )
      : unit = 
  let

    val resultFile = getResultLog( group, what )
     
    fun iterateTests(remaining : int) : bool =
    let
      val args = genInput( num-remaining )

      fun testAgainstLast (imp, (anyError, last)) =
        if List.null last then
          (false, [imp args])
        else
          let
            val cur = imp args
            val lastRes = List.hd last
            val different = not (compare (cur, lastRes))
          in
             (anyError orelse different, [cur])
          end
            
      val (anyError, _) = List.foldl 
        testAgainstLast (false, []) fs

      val _ = addResult( resultFile, inputToString args, not anyError )
    in
      if remaining > 0 then 
        (not anyError) andalso iterateTests(remaining - 1)
      else 
        (not anyError)
    end

    val success = iterateTests num

    val _ = print (group ^ ": " ^ what ^ " : " ^ resultToString success ^ "\n" )
  in
    ()
  end

  fun test' ( groups : string list )
            ( spec : { 
                group : string,
                what : string,
                num : int,
                genInput : int -> 'a,
                fs : ('a -> 'b) list,
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
