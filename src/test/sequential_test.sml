(*
* filename: sequential_test.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains sequential tests.
*)

structure SequentialTest =
struct

  fun runTest(
   spec : {
      group : string,
      what : string,

      numberOfTests : int,
      testGenerator : int -> ('a * 'b),
      implementation : 'a -> 'b,

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
      implementation = implementation,
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
      val (args, expected) = testGenerator( numberOfTests - remaining )
   
      val result = implementation args
      val success = compareResult (result, expected)

      val _ = saveResult(success, args)
    in
      if remaining > 0 then 
        success andalso iterateTests(remaining - 1)
      else 
        success
    end

    val _ = clearOldResult()

    val success = iterateTests numberOfTests

    val status = if success then "OK" else "Failed"
    val _ = print (group ^ ": " ^ what ^ " : " ^ status)
  in
    success
  end

end
