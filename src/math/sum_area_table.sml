(*
* file: sum_area_table.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality related to the sum area table.
* 
*)

signature SUM_AREA_TABLE =
sig

  type element
  type table
  type region = int * int * int * int

  val buildTable : int * int * ( int * int -> element ) -> table

  val sum : table -> region -> element

end

signature SUM_AREA_TABLE_SPEC =
sig
  type element

  val add : element * element -> element
  val subtract : element * element -> element 
  val zero : element

end

functor SumAreaTableFun ( spec : SUM_AREA_TABLE_SPEC ) : SUM_AREA_TABLE =
struct

  type element = spec.element
  type table = element Array2.array
  type region = int * int * int * int
 
  fun buildTable( height : int, width : int, elem : ( int * int ) -> element ) 
    : table =
  let
      val intImage = Array2.array( height, width, spec.zero )
      val add = spec.add
      val subtract = spec.subtract

      fun calculateIntElement( 0, 0, _ ) = elem( 0, 0 )
      |   calculateIntElement( i, 0, _ ) = 
            add( Array2.sub( intImage, i-1, 0 ), elem( i, 0 ) )
      |   calculateIntElement( 0, j, _ ) = 
            add( Array2.sub( intImage, 0, j-1 ), elem( 0, j ) )
      |   calculateIntElement( i, j, _ ) = 
            add(
              subtract( 
                add( 
                  Array2.sub( intImage, i, j-1 ),
                  Array2.sub( intImage, i-1, j ) ),
                Array2.sub( intImage, i-1, j-1) ), 
               elem( i, j ) )

      val range = { base = intImage, 
                    row = 0, 
                    col = 0, 
                    nrows = NONE, 
                    ncols = NONE }
      val _ = Array2.modifyi Array2.RowMajor calculateIntElement range
  in
    intImage
  end

  fun sum ( t : table ) ( ( i, j, height, width ) : region ) : element =
  let
    val add = spec.add
    val subtract = spec.subtract
    fun getValue( i, j ) =
      if i < 0 orelse i < 0 then spec.zero
      else Array2.sub(t, i, j)

  in
    subtract(
      add( getValue( i-1, j-1 ), getValue( i+height-1, j+width-1 ) ),
      add( getValue( i+height-1, j-1 ), getValue( i-1, j+width-1 ) ) )
  end

end

local
  structure IntSumAreaTableSpec : SUM_AREA_TABLE_SPEC =
  struct
  
    type element = int

    val add = Int.+
    val subtract = Int.-
    val zero = 0

  end

  structure RealSumAreaTableSpec : SUM_AREA_TABLE_SPEC =
  struct
  
    type element = real

    val add = Real.+
    val subtract = Real.-
    val zero = 0.0

  end

in
  structure IntSumAreaTable = SumAreaTableFun ( IntSumAreaTableSpec )
  structure RealSumAreaTable = SumAreaTableFun ( RealSumAreaTableSpec )
end
