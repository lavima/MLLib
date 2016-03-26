(*
* filename: complex.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains signature and structure with implementation for
* complex numbers.
*)


signature COMPLEX = sig
 type number
 
 val complex : real * real -> number
 
 val negative : number -> number
 val plus : number -> number -> number
 val minus : number -> number -> number
 val times : number -> number -> number
 val divide : number -> number -> number
 val invert : number -> number
 val exp : number -> number
 val re : number -> real
 val im : number -> real
 val toString : number -> string
end;
 
structure Complex :> COMPLEX = struct
  type number = real * real
 
  fun complex (a, b) = (a, b)
 
  fun negative (a, b) = 
     (~a, ~b)
  
  fun plus (a1, b1) (a2, b2) = 
    (Real.+ (a1, a2), Real.+(b1, b2))

  fun minus i1 i2 = 
    plus i1 (negative i2)

  fun times (a1 : real, b1 : real) (a2 : real, b2 : real) = 
    ((a1 * a2) - (b1 * b2), (a1 * b2) + (a2 * b1))

  fun divide( ( a1, b1 ) ; complex, ( a2, b2 ) : complex ) : complex =
    let
      val magnitude = Real.+(Real.*(a2, a2), Real.*(b2, b2));
      val re = Real./(Real.+(Real.*(a1, a2), Real.*(b1, b2)) ,magnitude)
      val im = Real./(Real.-(Real.*(b1, a2), Real.*(a1, b2)) ,magnitude)
    in
       (re, im)
    end

  fun invert (a, b) =
    let
      val denom = a * a + b * b
    in
      (a / denom, ~b / denom)
    end;

  fun exp (a, b) =
    let
        val e_x = Math.exp(a);
    in
        ( ( e_x*Math.cos b ), ( e_x*Math.sin b ) )
    end;

  fun re (a, b) =
    a;

  fun im (a, b) =
    b;
 
  fun toString (a, b) =
    Real.toString(a) ^ " + " ^ Real.toString(b) ^ "i";

end;
