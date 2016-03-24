(*
* filename:randomTest.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains the spesefication to run random tests
*)

structure RandomTest =
struct

  fun runTest(
   spec : {
      group : string,
      what : string,

      numberOfTests : int,
      testGenerator : unit -> 'a,
      implementations : ('a -> 'b) list,

      compareResult : ('b * 'b) -> bool,

      argsToString : 'a -> string
    }
   ) = 
  let
     
     val {
      group = group,
      what = what,

      numberOfTests = numberOfTests,
      testGenerator = testGenerator,
      implementations = implementations,

      compareResult = compareResult,

      argsToString = argsToString
    } = spec

 
    val resultFile = "results/" ^ group ^ "_" ^ what ^ ".log"

    fun clearOldResult () =
      if OS.FileSys.access (resultFile, []) then
        OS.FileSys.remove resultFile
      else ()

    fun saveResult ( success : bool, args ) : unit =
    let
      val stream = TextIO.openAppend resultFile
      val line = (Bool.toString success) ^ "," ^ (argsToString args)
      val _ = TextIO.output(stream, line ^ "\n")
      val _ = TextIO.closeOut stream
    in
      ()
    end

    fun iterateTests(remaining : int) : bool =
    let
      val args = testGenerator()

      fun testAgainstLast (imp, (anyError, last)) =
        if List.null last then
          (false, [imp args])
        else
          let
            val cur = imp args
            val lastRes = List.hd last
            val different = not (compareResult (cur, lastRes))
          in
             (anyError orelse different, [cur])
          end
            
      val (anyError, _) = List.foldl 
        testAgainstLast (false, []) implementations

      val _ = saveResult(not anyError, args)
    in
      if remaining > 0 then 
        (not anyError) andalso iterateTests(remaining - 1)
      else 
        (not anyError)
    end

    val _ = clearOldResult()

    val success = iterateTests numberOfTests

    val status = if success then "OK" else "Failed"
    val _ = print (group ^ ": " ^ what ^ " : " ^ status)
  in
    success
  end

end
