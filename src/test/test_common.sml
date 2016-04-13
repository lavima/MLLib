(*
* filename: test_common.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no> 
*
* This file contains a structure with common test functionality.
*)

structure TestCommon =
struct

    fun exnToString e = 
      "[" ^ exnName e ^ " " ^ exnMessage e ^ "]"

    fun resultToString false = "Failed"
      | resultToString true = "OK"
      
    fun getResultLog( group : string, what : string ) : TextIO.outstream =
    let
      val _ = 
        if not( OS.FileSys.isDir"results" ) then
          OS.FileSys.mkDir"results"
        else
          ()
        handle SysErr =>  
          OS.FileSys.mkDir"results"

      fun underscore( text : string ) : string = 
        String.implode(
          List.map
            ( fn c => if Char.isSpace c then #"_" else c )
            ( String.explode text ) )
    in
      TextIO.openOut( 
        "results/" ^ underscore group ^ "_" ^ underscore what ^ ".log" )
    end

    fun addResult( out : TextIO.outstream, inputString : string, result : bool )
        : unit =
      TextIO.output( 
        out, 
        inputString ^ " " ^ resultToString result )
    
    val isGroupMember = 
      ListUtil.member ( fn( a, b ) => String.compare( a, b )=EQUAL )

end (* structure TestCommon *)
