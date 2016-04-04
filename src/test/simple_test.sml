(* 
* file: simple_test.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure for performing simple unit tests.
*)

signature SIMPLE_TEST =
sig

  val test :  { group : string, 
                what : string, 
                genInput : unit -> 'a list,
                f : 'a list -> 'b list,
                evaluate : 'b list -> bool list,
                inputToString : 'a -> string }
              ->
              unit

  val test' : string list 
              ->
              { group : string, 
                what : string, 
                genInput : unit -> 'a list,
                f : 'a list -> 'b list,
                evaluate : 'b list -> bool list,
                inputToString : 'a -> string }
              ->
              unit
end

structure SimpleTest : SIMPLE_TEST = 
struct

  open TestCommon

  fun test( { group : string,
              what : string, 
              genInput : unit -> 'a list,
              f : 'a list -> 'b list, 
              evaluate : 'b list -> bool list,
              inputToString : 'a -> string } )
      : unit =
  let
    val resultFile = getResultLog( group, what ) 

    val input = genInput()
    val result = evaluate( f input )
      handle e => 
        ( print( "Unhandled exception:\n" ^ ( exnToString e ) ); 
          List.tabulate( List.length input, fn _ => false ) )

    val success = 
      List.foldl
        ( fn( ( i, r ), s ) => (
            addResult( resultFile, inputToString i, r );
            print( group ^ ": " ^ what ^ " : " ^ resultToString r );
            s andalso r ) )
        true
        ( ListPair.zip( input, result ) )

    val _ = print (group ^ ": " ^ what ^ " : " ^ resultToString success )
  in
    ()
  end

  fun test' ( groups : string list )
            ( spec : { 
                group : string, 
                what : string, 
                genInput : unit -> 'a list,
                f : 'a list -> 'b list,
                evaluate : 'b list -> bool list,
                inputToString : 'a -> string } )
      : unit =
    case groups of
      [] => test spec
    | _ => 
      case isGroupMember( #group spec, groups ) of
        false => ()
      | true => test spec

end
