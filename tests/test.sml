(* 
* file: test.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains shared test functionality.
*)

(* 
* Polymorphic function for running and evaluating the outcome of a test.
*)


fun test( Text: string, 
          run : unit -> 'a, 
          evaluate : 'a -> bool ) : bool =
let
   fun exnToString(e) =
        List.foldr (op ^) "" [ "[",
                              exnName e,
                              " ",
                              exnMessage e,
                              "]"]


  val _ = print( Text ^ ": " )

  val Success = evaluate( run() )
    handle e => ( 
      print( "Unhandled exception:\n" ^ ( exnToString e ) ); 
      false )
in
  if Success then
    ( print"OK\n"; true )
  else
    ( print"ERROR\n"; false )
end


 

