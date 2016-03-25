(*
* file: matlab.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file contains functionality for integrating with matlab.
*)

structure Matlab = 
struct
  fun executeMatlabFunction(call : string) : unit =
  let
    val command = 
      "-nojvm -nodisplay -nosplash -r \"" ^ call ^ "; quit\" > /dev/null 2>&1"
    val process = Unix.execute ("/usr/bin/matlab", [command])
    val status = Unix.reap process
  in
    ()
  end

end
