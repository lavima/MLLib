(*
* file: list_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains helpful list related functions that cannot be found in the
* SML BASIS library.
*)

structure ListUtil =
struct

  fun appi ( f : int * 'a -> unit ) ( xs : 'a list ) : unit =
  let
    fun appi'( i : int, xs' ) : unit =
      case xs' of 
        [] => ()
      | x::rxs' => ( f( i, x ); appi'( i+1, rxs' ) )
  in
    appi'( 0, xs )
  end

  fun mapi ( f : int * 'a -> 'b ) ( xs : 'a list ) 
      : 'b list =
  let
    fun mapi'( i : int, xs' ) : 'b list =
      case xs' of
        [] => []
      | x::rxs' => f( i, x )::mapi'( i+1, rxs' )
  in
    mapi'( 0, xs )
  end

  fun foldli ( f : int * 'a * 'b -> 'b ) ( init : 'b ) ( xs : 'a list ) 
      : 'b =
  let
    fun foldli' ( i : int, accum : 'b, xs' ) : 'b =
      case xs' of
        [] => accum
      | x::rxs' => foldli'( i+1, f( i, x, accum ), rxs' )
  in
    foldli'( 0, init, xs )
  end


  fun same ( eq : 'a * 'a -> bool ) ( xs : 'a list ) : bool =
    case xs of
      [] => true
    | [ x ] => true
    | x::rxs => 
        List.foldl 
          ( fn( y, same ) => 
              if eq( x, y ) then
                same
              else
                false )
          true
          rxs

  (*
  * Determine whether two lists are identical. 
  * Deprecated: Use ListPair.allEq instead.
  *)
  fun equal ( eq : 'a * 'a -> bool ) ( xs : 'a list, ys : 'a list ) : bool =
  let
    fun equal'( [], [] ) = true
      | equal'( [], _ ) = false
      | equal'( _, [] ) = false
      | equal'( x::xs', y::ys' ) = eq( x, y ) andalso equal'( xs', ys' )
  in
    equal'( xs, ys )
  end
  
  (*
  * Determine whether an item is part of a list or not.
  *)
  fun member( eq : 'a * 'a -> bool ) ( y : 'a, xs : 'a list ) : bool =
    case xs of 
      [] => false
    | x::rxs => 
        if eq( y, x ) then
          true
        else
          member eq ( y, rxs )

  (*
  *  Remove the first occurrence of y from xs 
  *)
  fun remove( eq : 'a * 'a -> bool ) ( y : 'a, xs : 'a list ) : 'a list =
    case xs of 
      [] => [] 
    | x::rxs => 
        if eq( y, x ) then
          rxs
        else
          x::( remove eq ( y, rxs ) )

  (* 
  * Retrieve the unique elements of a list. 
  *)
  fun unique( eq : 'a * 'a -> bool ) ( xs : 'a list ) : 'a list =
    case xs of
      [] => []
    | x::rxs =>
        case unique eq rxs of ys =>
        if member eq ( x, ys ) then
          ys
        else
          x::ys 

  (*
  * Generate a list of elements with the  specified increment in the specified 
  * interval.
  * TODO Refactor to use add instead of inc so that the increments can be 
  * specified in last tuple.
  *)
  fun fromTo ( inc : 'a -> 'a, less : 'a * 'a -> bool ) ( L : 'a, H : 'a ) 
      : 'a list =
  let
    fun fromTo'( i : 'a ) : 'a list =
      case less( H, i ) of
        true => []
      | false => i::fromTo'( inc i )
  in
    fromTo' L
  end

  val fromToInt = fromTo ( fn x => x+1, Int.< )
  val fromToReal = fromTo ( fn x => x+1.0, Real.< )


  (*
  * Combine two list into a list of two tuples
  *
  * The two lists must be equal in length, or else the function will throw an 
  * unmatched exception
  *)
  fun combine( xs : 'a list, ys : 'b list ) : ( 'a * 'b ) list = 
    case ( xs, ys ) of
      ( [], [] ) => []
    | ( x::rxs, y::rys ) => ( x, y )::combine( rxs, rys )

  (*
  * Combine three list into a list of three tuples
  *
  * The three lists must be equal in length, or else the function will throw an 
  * unmatched exception
  *)
  fun combine3( xs : 'a list, ys : 'b list, zs : 'c list ) 
      : ( 'a * 'b * 'c ) list = 
    case ( xs, ys, zs ) of
      ( [], [], [] ) => []
    | ( x::rxs, y::rys, z::rzs ) => ( x, y, z )::combine3( rxs, rys, rzs )


  fun binaryOp ( op' : 'a * 'a -> 'a ) ( xs : 'a list, ys : 'a list ) 
      : 'a list =
    case ( xs, ys ) of 
      ( [], [] ) => []
    | ( x::xs', y::ys' ) => op'( x, y )::binaryOp op' ( xs', ys' )

  fun flatMap ( f : 'a -> 'b ) ( xss : 'a list list ) : 'b list =
    case xss of
      [] => []
    | xs::xss' => ( List.map f xs ) @ ( flatMap f xss' )

  fun nestMap ( f : 'a -> 'b ) ( columns : int, xs : 'a list ) : 'b list list =
  let
    fun buildRow( 0, rest : 'a list ) = ( [], rest )
      | buildRow( remainingColumns : int, elem::rest : 'a list ) = 
    let
      val ( row, remainingData ) = buildRow( remainingColumns-1, rest )
    in
      ( ( f elem )::row, remainingData )
    end

    fun buildRows( [] : 'a list ) = []
      | buildRows( data : 'a list ) =
    let
      val ( row, remaining ) = buildRow( columns, data )
    in
      row::( buildRows remaining ) 
    end
  in
    buildRows xs
  end

  fun toString ( tos : 'a -> string )
               ( xs : 'a list ) 
      : string =
  let
    fun iter( xs : 'a list ) : string list =
      case xs of
        [] => []
      | [ x ] => [ tos x ]
      | x::rxs => ( tos x ^ ", " )::iter rxs
  in
    "[ " ^ String.concat( iter xs ) ^ " ]"
  end

end (* structure ListUtil *)
