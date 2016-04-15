(*
* file: math.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains a structure with mathematical utilities
*)

structure MathUtil =
struct
  
  fun avg( xs : real list ) : real =
    ( List.foldl ( fn( x, sum ) => sum+x ) 0.0 xs ) / real( List.length xs )

  fun lerp( x : real, y : real, t : real ) : real =
    x*(1.0-t)+y*t

  
  fun blerp( x1 : real, x2 : real, y1 : real, y2 : real, t1 : real, t2 : real )
      : real =
    ( t1 - 1.0 )*( t2 - 1.0 )*x1 + 
    t1*( 1.0 - t2 )*x2 + 
    t2*( 1.0 - t1 )*y1 + 
    t1*t2*y2

  fun chiSquared( histogram1 : real array, histogram2 : real array ) =
  let
     fun element( t, b ) = 
       if (t+b<0.00001) then 0.0
       else ( Math.pow(t-b, 2.0))/(t+b)
        
     val sum = Array.foldli
       ( fn ( i, x, a ) => element( x, Array.sub( histogram2, i ) ) + a )
       ( 0.0 )
       histogram1
  in
    0.5*sum
  end


end (* struct MathUtil *)
