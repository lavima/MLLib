(*
* file: list_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains helpful list related functions that cannot be found in the
* SML BASIS library.
*)

structure ListUtil =
struct

  fun appi ( f : int * 'a -> unit ) ( Xs : 'a list ) : unit =
  let
    fun appi'( I : int, Xs' ) : unit =
      case Xs' of 
        [] => ()
      | X::RXs' => ( f( I, X ); appi'( I+1, RXs' ) )
  in
    appi'( 0, Xs )
  end

  fun mapi ( f : int * 'a -> 'b ) ( Xs : 'a list ) 
      : 'b list =
  let
    fun mapi'( I : int, Xs' ) : 'b list =
      case Xs' of
        [] => []
      | X::RXs' => f( I, X )::mapi'( I+1, RXs' )
  in
    mapi'( 0, Xs )
  end

  fun foldli ( f : int * 'a * 'b -> 'b ) ( Init : 'b ) ( Xs : 'a list ) 
      : 'b =
  let
    fun foldli' ( I : int, Accum : 'b, Xs' ) : 'b =
      case Xs' of
        [] => Accum
      | X::RXs' => foldli'( I+1, f( I, X, Accum ), RXs' )
  in
    foldli'( 0, Init, Xs )
  end

  fun same ( eq : 'a * 'a -> bool ) ( Xs : 'a list ) : bool =
    case Xs of
      [] => true
    | [ X ] => true
    | X::RXs => 
        List.foldl 
          ( fn( Y, Same ) => 
              if eq( X, Y ) then
                Same
              else
                false )
          true
          RXs

  (*
  * Determine whether two lists are identical. 
  *)
  fun equal ( eq : 'a * 'a -> bool ) ( Xs : 'a list, Ys : 'a list ) : bool =
  let
    fun equal'( [], [] ) = true
      | equal'( [], _ ) = false
      | equal'( _, [] ) = false
      | equal'( X::Xs', Y::Ys' ) = eq( X, Y ) andalso equal'( Xs', Ys' )
  in
    equal'( Xs, Ys )
  end
  
  (*
  * Determine whether an item is part of a list or not.
  *)
  fun member( eq : 'a * 'a -> bool ) ( Y : 'a, Xs : 'a list ) : bool =
    case Xs of 
      [] => false
    | X::RXs => 
        if eq( Y, X ) then
          true
        else
          member eq ( Y, RXs )

  (*
  *  Remove the first occurrence of Y from Xs 
  *)
  fun remove( eq : 'a * 'a -> bool ) ( Y : 'a, Xs : 'a list ) : 'a list =
    case Xs of 
      [] => [] 
    | X::RXs => 
        if eq( Y, X ) then
          RXs
        else
          X::( remove eq ( Y, RXs ) )

  (* 
  * Retrieve the unique elements of a list. 
  *)
  fun unique( eq : 'a * 'a -> bool ) ( Xs : 'a list ) : 'a list =
    case Xs of
      [] => []
    | X::RXs =>
        case unique eq RXs of Ys =>
        if member eq ( X, Ys ) then
          Ys
        else
          X::Ys 

  (*
  * Generate a list of elements with the  specified increment in the specified 
  * interval.
  * TODO Refactor to use add instead of inc so that the increments can be 
  * specified in last tuple.
  *)
  fun fromTo ( inc : 'a -> 'a, less : 'a * 'a -> bool ) ( L : 'a, H : 'a ) 
      : 'a list =
  let
    fun fromTo'( I : 'a ) : 'a list =
      case less( H, I ) of
        true => []
      | false => I::fromTo'( inc I )
  in
    fromTo' L
  end

  val fromToInt = fromTo ( fn X => X+1, Int.< )
  val fromToReal = fromTo ( fn X => X+1.0, Real.< )


  (*
  * Combine two list into a list of two tuples
  *
  * The two lists must be equal in length, or else the function will throw an 
  * unmatched exception
  *)
  fun combine( Xs : 'a list, Ys : 'b list ) : ( 'a * 'b ) list = 
    case ( Xs, Ys ) of
      ( [], [] ) => []
    | ( X::RXs, Y::RYs ) => ( X, Y )::combine( RXs, RYs )

  (*
  * Combine three list into a list of three tuples
  *
  * The three lists must be equal in length, or else the function will throw an 
  * unmatched exception
  *)
  fun combine3( Xs : 'a list, Ys : 'b list, Zs : 'c list ) 
      : ( 'a * 'b * 'c ) list = 
    case ( Xs, Ys, Zs ) of
      ( [], [], [] ) => []
    | ( X::RXs, Y::RYs, Z::RZs ) => ( X, Y, Z )::combine3( RXs, RYs, RZs )


  fun binaryOp ( op' : 'a * 'a -> 'a ) ( Xs : 'a list, Ys : 'a list ) 
      : 'a list =
    case ( Xs, Ys ) of 
      ( [], [] ) => []
    | ( X::Xs', Y::Ys' ) => op'( X, Y )::binaryOp op' ( Xs', Ys' )

  fun toString ( toString : 'a -> string )
               ( Xs : 'a list ) 
      : string =
  let
    fun iter( Xs : 'a list ) : string =
      case Xs of
        [ X ] => toString X
      | X::RXs => toString X ^ ", " ^ iter RXs 
  in
    "[ " ^ iter Xs ^ " ]" 
  end


end (* structure ListUtil *)
