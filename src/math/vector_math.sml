(*
* file: vector_math.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file contains mathematical operations for dealing with vectors.
*)

structure VectorMath =
struct
  fun addReal( v1 : real vector, v2 : real vector ) : real vector =
    Vector.tabulate( Vector.length v1, 
      fn i => Vector.sub( v1, i ) + Vector.sub ( v2, i ) )

  fun subtractReal( v1 : real vector, v2 : real vector ) : real vector =
    Vector.tabulate( Vector.length v1, 
      fn i => Vector.sub( v1, i ) - Vector.sub ( v2, i ) )

  fun multiplyScalarReal( v : real vector, s : real ) : real vector =
    Vector.map ( fn x => x * s ) v


end
