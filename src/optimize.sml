(*
* file: optimize.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with functionality for optimization.
*)

structure Optimize =
struct
  
  datatype bruteResolution = 
    full of int         (* Do a full iteration along a dimensions *)
  | lessEqual of int    (* Only test values less or equal to the previous 
                           dimension *)


  fun brute ( Dimensions : int, Resolutions : bruteResolution list )
            ( f : real list -> real ) 
      : real list =
  let 

    fun getCount( Res : bruteResolution ) : int =
      case Res of 
        full X => X
      | lessEqual X => X

    fun checkRes( Index : int, 
                  Res : bruteResolution, 
                  Arg : real, 
                  Args : real list ) 
        : bool =
      case Index<=( getCount Res ) of
        false => false 
      | true =>
          case Res of
            full _ => true
          | lessEqual _ => 
              case Args of 
                [] => true
              | PrevArg::_ => Arg<=PrevArg


    val Max = ref 0.0
    val Values = ref []

    fun iterate( Dimension : int, 
                 Ress : bruteResolution list,
                 Args : real list ) 
        : unit =
    let 
      val Res::RRess = Ress

      val Delta = 1.0/( real( getCount Res ) )

      fun iterate'( Index : int ) 
          : unit =
        case ( real Index )*Delta of Arg =>
        case checkRes( Index, Res, Arg, Args ) of 
          false => () 
        | true => 
          let
            val Args' = Arg::Args
          in
            case Dimension<( Dimensions-1 ) of
              false =>
              let
                val Eval = f( Args' )
                val _ =
                  if Eval>( !Max ) then (
                    Max := Eval;
                    Values := Args' )
                  else
                    ()
              in
                iterate'( Index+1 )
              end
            | true => (
                iterate( Dimension+1, RRess, Args' );
                iterate'( Index+1 ) )
          end
    in
      iterate' 0
    end
  in
    ( iterate( 0, Resolutions, [] ); !Values )
  end

end (* structure Optimize *)
