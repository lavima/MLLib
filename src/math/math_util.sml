(*
* file: math.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains a structure with mathematical utilities
*)

structure MathUtil =
struct
  
  fun avg( Xs : real list ) : real =
    ( List.foldl ( fn( X, Sum ) => Sum+X ) 0.0 Xs ) / real( List.length Xs )

  fun lerp( X : real, Y : real, T : real ) : real =
    X*(1.0-T)+Y*T

  
  fun blerp( X1 : real, X2 : real, Y1 : real, Y2 : real, T1 : real, T2 : real )
      : real =
    ( T1 - 1.0 )*( T2 - 1.0 )*X1 + 
    T1*( 1.0 - T2 )*X2 + 
    T2*( 1.0 - T1 )*Y1 + 
    T1*T2*Y2

end (* struct MathUtil *)
