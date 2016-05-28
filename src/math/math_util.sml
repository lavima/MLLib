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
     val sum = Array.foldli
       ( fn ( i, h1, a ) =>
         let
           val h2 = Array.sub( histogram2, i ) 
         in
           if Real.==(h1+h2, 0.0) then 0.0
           else ( Math.pow(h1-h2, 2.0))/(h1+h2)
         end +a )
       ( 0.0 )
       histogram1
  in
    sum/2.0
  end


end (* struct MathUtil *)
