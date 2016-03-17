(*
* file: general.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains helpful functions that cannot be found in the SML BASIS
* library.
*)

structure Util =
struct

  (* 
  * Determine whether two reals are approximately equal.
  *)
  fun approxEqReal( x : real, y : real, precision : int ) : bool =
  let
    val p = Math.pow( 10.0, real precision )
    val x' = trunc( x*p )
    val y' = trunc( y*p )
  in
    x'=y'
  end

  (* 
  * Determine whether two reals are approximately equal by 
  * checking the difference. This is tested up to 
  *)
  fun approxEqReal'( x : real, y : real, precision : int ) : bool =
  let
    val e = Real./(1.0, Math.pow(10.0, real precision));
    val residual = Real.abs(Real.-(x, y))
  in
    Real.<(residual, e)
  end


  (*
  * Curried function for defining eq functions from a compare function.
  *)
  fun eq ( compare : 'a * 'a -> order ) ( x : 'a, y : 'a ) : bool = 
    case compare( x, y ) of 
      EQUAL => true
    | _ => false

  
  val eqInt = eq Int.compare

  (* TODO put all loop related functions into a Loop structure and add support 
     for loopN functionality like in Optimize.brute *)

  fun loopFromTo ( less : 'a * 'a -> bool, add : 'a * 'a -> 'a )
                 ( f : 'a -> unit )
                 ( from : 'a, to : 'a, inc : 'a )
      : unit =
  let
    fun loop( i : 'a ) : unit =
      case less( to, i ) of 
        false => ( f i; loop( add( i, inc ) ) )
  in
    loop from
  end

  val loopFromToReal = loopFromTo ( Real.<, Real.+ )

  fun accumLoopFromTo ( less : 'a * 'a -> bool, add : 'a * 'a -> 'a )
                      ( f : 'a * 'b -> 'b )
                      ( start : 'b )
                      ( from : 'a, to : 'a, inc : 'a )
      : 'b =
  let
    fun loop( i : 'a, x : 'b ) : 'b =
      case less( to, i ) of 
        false => loop( add( i, inc ), f( i, x ) )
      | true => x
  in
    loop( from, start )
  end

  val accumLoopFromToReal = accumLoopFromTo ( Real.<, Real.+ )

  (*
  * Function for iterating in a loop with an increasing index variable. The 
  * index values start at 0 and ends at count-1.
  *)
  fun loop ( f : int -> unit ) ( count : int ) : unit =
  let
    fun loop'( i : int ) : unit =
      case i<count of
        false => ()
      | true => ( f i; loop'( i+1 ) )
  in
    loop' 0
  end

  fun accumLoop ( f : int * 'a -> 'a ) ( init : 'a ) ( count : int ) : 'a =
  let
    fun loop'( i : int, accum : 'a ) : 'a =
      case i<count of
        false => accum 
      | true => loop'( i+1, f( i, accum ) )
  in
    loop'( 0, init )
  end

  (*
  * Retrieve the largest element of two elements.
  *)
  fun max2 ( less : 'a * 'a -> bool )
           ( x : 'a, y : 'a ) : 'a =
    if less( x, y ) then 
      y 
    else 
      x

  val max2Int = max2 Int.<
  val min2Int = max2 Int.>

  (*
  * Retrieve the largest element in a list.
  *
  * The function assumes that the list has at least one element. If not, a
  * match exception will occur.
  *)
  fun max ( less : 'a * 'a -> bool ) 
          ( xs : 'a list ) : 'a =
  let
    val x::rxs = xs

    fun max'( ys : 'a list, max : 'a ) : 'a =
      case ys of 
        [] => max
      | y::rys => 
          max'( rys, max2 less ( y, max ) )
  in
    max'( rxs, x ) 
  end

  val maxInt = max Int.<
  val minInt = max Int.>

  fun avg ( add : 'a * 'a -> 'a, divide : 'a * int -> 'a, zero : unit -> 'a )
          ( xs : 'a list ) : 'a =
    divide( ( List.foldl ( fn( x, sum ) => add( x, sum ) ) ( zero() ) xs ), 
            List.length xs )


end (* structure Util *)
