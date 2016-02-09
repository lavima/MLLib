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


  fun brute ( dimensions : int, resolutions : bruteResolution list )
            ( f : real list -> real ) 
      : real list =
  let 

    fun getCount( res : bruteResolution ) : int =
      case res of 
        full x => x
      | lessEqual x => x

    fun checkRes( index : int, 
                  res : bruteResolution, 
                  arg : real, 
                  args : real list ) 
        : bool =
      case index<=( getCount res ) of
        false => false 
      | true =>
          case res of
            full _ => true
          | lessEqual _ => 
              case args of 
                [] => true
              | prevArg::_ => arg<=prevArg


    val max = ref 0.0
    val values = ref []

    fun iterate( dimension : int, 
                 ress : bruteResolution list,
                 args : real list ) 
        : unit =
    let 
      val res::rress = ress

      val delta = 1.0/( real( getCount res ) )

      fun iterate'( index : int ) 
          : unit =
        case ( real index )*delta of arg =>
        case checkRes( index, res, arg, args ) of 
          false => () 
        | true => 
          let
            val args' = arg::args
          in
            case dimension<( dimensions-1 ) of
              false =>
              let
                val eval = f( args' )
                val _ =
                  if eval>( !max ) then (
                    max := eval;
                    values := args' )
                  else
                    ()
              in
                iterate'( index+1 )
              end
            | true => (
                iterate( dimension+1, rress, args' );
                iterate'( index+1 ) )
          end
    in
      iterate' 0
    end
  in
    ( iterate( 0, resolutions, [] ); !values )
  end

end (* structure Optimize *)
