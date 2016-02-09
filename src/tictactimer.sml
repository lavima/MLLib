signature TICTACTIMER = 
sig
  type tictactimer 
  val tic : string -> tictactimer  
  val tac : tictactimer  -> unit
end

structure TicTacTimer:TICTACTIMER =
struct

  type tictactimer  = string * Timer.cpu_timer * Timer.real_timer

  fun tic( name : string ) : tictactimer =
    ( name, Timer.startCPUTimer(), Timer.startRealTimer() )

  fun tac( timer as ( name, cpuTimer, realTimer ) : tictactimer ) : unit =
  let
    val { nongc = { usr = u, sys = s }, 
          gc = { usr = gcu, sys = gcs } } = 
      Timer.checkCPUTimes cpuTimer
    val r = Timer.checkRealTimer realTimer 
  in
    ( print( "** " ^ name ^ " **\n" );
      print( "Real: " ^ Time.toString r ^ "\n" );
      print( "CPU: " ^ Time.toString u ^ " " ^ Time.toString s ^ "\n" );
      print( "GC: " ^ Time.toString gcu ^ " " ^ Time.toString gcs ^ "\n" );
      print( "\n" ) )
  end

end
