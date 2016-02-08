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

  type 'a array = { Width : int, Height : int, Values : 'a Array.array }

  fun array( Width : int, Height : int, Val : 'a ) : 'a array = 
    { Width=Width, Height=Height, Values = Array.array( Width*Height, Val ) }

  fun fromList( Width : int, Height : int, Vals : 'a list ) : 'a array =
    if List.length Vals=Width*Height then
      { Width=Width, Height=Height, Values=Array.fromList Vals }
    else
      raise Size


  fun sub( Array as { Width, Values, ... } : 'a array, I : int, J : int ) 
      : 'a =
    Array.sub( Values, I*Width+J )

  fun sub'( Array as { Values, ... } : 'a array, I : int ) : 'a =
    Array.sub( Values, I )


  fun update( Array as { Width, Values, ... } : 'a array, 
              I : int, J : int, Val : 'a ) 
      : unit =
    Array.update( Values, I*Width+J, Val )

  fun update'( Array as { Width, Values, ... } : 'a array, 
               I : int, Val : 'a ) 
      : unit =
    Array.update( Values, I, Val )


  fun app ( f : 'a -> unit )
          ( Array as { Width, Height, Values } : 'a array )
      : unit =
    Array.app f Values

  fun appi ( f : int * 'a -> unit )
           ( Array as { Width, Height, Values } : 'a array )
      : unit =
    Array.appi f Values

  fun appij ( f : int * int * 'a -> unit )
            ( Array as { Width, Height, Values } : 'a array )
      : unit =
    Array.appi ( fn( I, Pixel ) => f( I mod Width, I div Width, Pixel ) ) Values

  fun foldl ( f : 'a * 'b -> 'b )
            ( Start : 'b )
            ( Array as { Width, Height, Values } : 'a array ) : 'b =
    Array.foldl f Start Values

  fun foldli ( f : int * 'a * 'b -> 'b )
             ( Start : 'b )
             ( Array as { Width, Height, Values } : 'a array ) : 'b =
    Array.foldli f Start Values

  fun foldlij ( f : int * int * 'a * 'b -> 'b )
             ( Start : 'b )
             ( Array as { Width, Height, Values } : 'a array ) : 'b =
    Array.foldli 
      ( fn( I, Pixel, X ) => f( I div Width, I mod Width, Pixel, X ) ) 
      Start 
      Values

  fun foldr ( f : 'a * 'b -> 'b )
            ( Start : 'b )
            ( Array as { Width, Height, Values } : 'a array ) : 'b =
    Array.foldr f Start Values

  fun foldri ( f : int * 'a * 'b -> 'b )
             ( Start : 'b )
             ( Array as { Width, Height, Values } : 'a array ) : 'b =
    Array.foldri f Start Values

  fun foldrij ( f : int * int * 'a * 'b -> 'b )
             ( Start : 'b )
             ( Array as { Width, Height, Values } : 'a array ) : 'b =
    Array.foldri 
      ( fn( I, Pixel, X ) => f( I div Width, I mod Width, Pixel, X ) ) 
      Start 
      Values

  fun modify ( f : 'a -> 'a )
             ( Array as { Width, Height, Values } : 'a array ) 
      : unit =
    Array.modify f Values

  fun modifyi ( f : int * 'a -> 'a )
             ( Array as { Width, Height, Values } : 'a array ) 
      : unit =
    Array.modifyi f Values

  fun modifyij ( f : int * int * 'a -> 'a )
             ( Array as { Width, Height, Values } : 'a array ) 
      : unit =
    Array.modifyi 
      ( fn( I, Pixel ) => f( I div Width, I mod Width, Pixel ) ) 
      Values

end (* structure Array2D *)
