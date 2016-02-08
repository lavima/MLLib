signature TICTACTIMER = 
sig
  type tictactimer 
  val tic : string -> tictactimer  
  val tac : tictactimer  -> unit
end

structure TicTacTimer:TICTACTIMER =
struct

  type tictactimer  = string * Timer.cpu_timer * Timer.real_timer

  fun tic( Name : string ) : tictactimer =
    ( Name, Timer.startCPUTimer(), Timer.startRealTimer() )

  fun tac( Timer as ( Name, CPUTimer, RealTimer ) : tictactimer ) : unit =
  let
    val { nongc = { usr = U, sys = S }, 
          gc = { usr = GCU, sys = GCS } } = 
      Timer.checkCPUTimes CPUTimer
    val R = Timer.checkRealTimer RealTimer 
  in
    ( print( "** " ^ Name ^ " **\n" );
      print( "Real: " ^ Time.toString R ^ "\n" );
      print( "CPU: " ^ Time.toString U ^ " " ^ Time.toString S ^ "\n" );
      print( "GC: " ^ Time.toString GCU ^ " " ^ Time.toString GCS ^ "\n" );
      print( "\n" ) )
  end

end
