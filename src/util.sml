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
  fun approxEqReal( X : real, Y : real, Precision : int ) : bool =
  let
    val Pow = Math.pow( 10.0, real Precision )
    val X' = trunc( X*Pow )
    val Y' = trunc( Y*Pow )
  in
    X'=Y'
  end

  (* 
  * Determine whether two reals are approximately equal by 
  * checking the difference. This is tested up to 
  * authod: Marius Geitle <marius.geitle@hiof.no>
  *)
  fun approxEqReal'( X : real, Y : real, precision : int ) : bool =
  let
    val e = Real./(1.0, Math.pow(10.0, real precision));
    val residual = Real.abs(Real.-(X, Y))
  in
    Real.<(residual, e)
  end


  (*
  * Curried function for defining eq functions from a compare function.
  *)
  fun eq ( compare : 'a * 'a -> order ) ( X : 'a, Y : 'a ) : bool = 
    case compare( X, Y ) of 
      EQUAL => true
    | _ => false

  val eqInt : int * int -> bool = eq Int.compare
  val eqReal : real * real -> bool = eq Real.compare

  (* TODO put all loop related functions into a Loop structure and add support 
     for loopN functionality like in Optimize.brute *)

  fun loopFromTo ( less : 'a * 'a -> bool, add : 'a * 'a -> 'a )
                 ( f : 'a -> unit )
                 ( From : 'a, To : 'a, Inc : 'a )
      : unit =
  let
    fun loop( I : 'a ) : unit =
      case less( To, I ) of 
        false => ( f I; loop( add( I, Inc ) ) )
  in
    loop From
  end

  val loopFromToReal = loopFromTo ( Real.<, Real.+ )

  fun accumLoopFromTo ( less : 'a * 'a -> bool, add : 'a * 'a -> 'a )
                      ( f : 'a * 'b -> 'b )
                      ( Start : 'b )
                      ( From : 'a, To : 'a, Inc : 'a )
      : 'b =
  let
    fun loop( I : 'a, X : 'b ) : 'b =
      case less( To, I ) of 
        false => loop( add( I, Inc ), f( I, X ) )
      | true => X
  in
    loop( From, Start )
  end

  val accumLoopFromToReal = accumLoopFromTo ( Real.<, Real.+ )

  (*
  * Function for iterating in a loop with an increasing index variable. The 
  * index values start at 0 and ends at Count-1.
  *)
  fun loop ( f : int -> unit ) ( Count : int ) : unit =
  let
    fun loop'( I : int ) : unit =
      case I<Count of
        false => ()
      | true => ( f I; loop'( I+1 ) )
  in
    loop' 0
  end

  fun accumLoop ( f : int * 'a -> 'a ) ( Init : 'a ) ( Count : int ) : 'a =
  let
    fun loop'( I : int, Accum : 'a ) : 'a =
      case I<Count of
        false => Accum 
      | true => loop'( I+1, f( I, Accum ) )
  in
    loop'( 0, Init )
  end

  (*
  * Retrieve the largest element of two elements.
  *)
  fun max2 ( less : 'a * 'a -> bool )
           ( X : 'a, Y : 'a ) : 'a =
    if less( X, Y ) then 
      Y 
    else 
      X

  val max2Int = max2 Int.<
  val min2Int = max2 Int.>

  (*
  * Retrieve the largest element in a list.
  *
  * The function assumes that the list has at least one element. If not, a
  * match exception will occur.
  *)
  fun max ( less : 'a * 'a -> bool ) 
          ( Xs : 'a list ) : 'a =
  let
    val X::RXs = Xs

    fun max'( Ys : 'a list, Max : 'a ) : 'a =
      case Ys of 
        [] => Max
      | Y::RYs => 
          max'( RYs, max2 less ( Y, Max ) )
  in
    max'( RXs, X ) 
  end

  val maxInt = max Int.<
  val minInt = max Int.>

  fun avg ( add : 'a * 'a -> 'a, divide : 'a * int -> 'a, zero : unit -> 'a )
          ( Xs : 'a list ) : 'a =
    divide( ( List.foldl ( fn( X, Sum ) => add( X, Sum ) ) ( zero() ) Xs ), 
            List.length Xs )


end (* structure Util *)
