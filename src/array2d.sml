(*
* file: array2d.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains a structure that represents a two dimensional array.
* The array is represented using a normal array which is zero indexed in 
* a row-major fashion. Index (i,j) is the i-th row and the j-th column. 
*)

structure Array2D =
struct

  type 'a array = { width : int, height : int, values : 'a Array.array }

  fun array( width : int, height : int, x : 'a ) : 'a array = 
    { width=width, height=height, values = Array.array( width*height, x ) }

  fun fromList( width : int, height : int, xs : 'a list ) : 'a array =
    if List.length xs=width*height then
      { width=width, height=height, values=Array.fromList xs }
    else
      raise Size


  fun sub( { width, values, ... } : 'a array, i : int, j : int ) 
      : 'a =
    Array.sub( values, i*width+j )

  fun sub'( { values, ... } : 'a array, i : int ) : 'a =
    Array.sub( values, i )


  fun update( { width, values, ... } : 'a array, i : int, j : int, x : 'a ) 
      : unit =
    Array.update( values, i*width+j, x )

  fun update'( { width, values, ... } : 'a array, i : int, x : 'a ) 
      : unit =
    Array.update( values, i, x )


  fun app ( f : 'a -> unit )
          ( { values, ... } : 'a array )
      : unit =
    Array.app f values

  fun appi ( f : int * 'a -> unit )
           ( { values, ... } : 'a array )
      : unit =
    Array.appi f values

  fun appij ( f : int * int * 'a -> unit )
            ( { width, height, values } : 'a array )
      : unit =
    Array.appi ( fn( i, pixel ) => f( i mod width, i div width, pixel ) ) values

  fun foldl ( f : 'a * 'b -> 'b )
            ( start : 'b )
            ( { values, ... } : 'a array ) : 'b =
    Array.foldl f start values

  fun foldli ( f : int * 'a * 'b -> 'b )
             ( start : 'b )
             ( { values, ... } : 'a array ) : 'b =
    Array.foldli f start values

  fun foldlij ( f : int * int * 'a * 'b -> 'b )
             ( start : 'b )
             ( { width, height, values } : 'a array ) : 'b =
    Array.foldli 
      ( fn( i, pixel, x ) => f( i div width, i mod width, pixel, x ) ) 
      start 
      values

  fun foldr ( f : 'a * 'b -> 'b )
            ( start : 'b )
            ( { values, ... } : 'a array ) : 'b =
    Array.foldr f start values

  fun foldri ( f : int * 'a * 'b -> 'b )
             ( start : 'b )
             ( { values, ... } : 'a array ) : 'b =
    Array.foldri f start values

  fun foldrij ( f : int * int * 'a * 'b -> 'b )
             ( start : 'b )
             ( { width, height, values } : 'a array ) : 'b =
    Array.foldri 
      ( fn( i, pixel, x ) => f( i div width, i mod width, pixel, x ) ) 
      start 
      values

  fun modify ( f : 'a -> 'a )
             ( { values, ... } : 'a array ) 
      : unit =
    Array.modify f values

  fun modifyi ( f : int * 'a -> 'a )
             ( { values, ... } : 'a array ) 
      : unit =
    Array.modifyi f values

  fun modifyij ( f : int * int * 'a -> 'a )
             ( { width, height, values } : 'a array ) 
      : unit =
    Array.modifyi 
      ( fn( i, pixel ) => f( i div width, i mod width, pixel ) ) 
      values

end (* structure Array2D *)
