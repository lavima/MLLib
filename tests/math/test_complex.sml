(* 
* file: test_complex.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the complex structure
*)


val _ = print"\n\n********** Complex number tests **********\n"

val _ = UnitTest.test( "negative",
  fn() => Complex.negative(Complex.complex(2.0, 3.0)),
  fn x =>
      Util.eqReal(Complex.re(x), ~2.0) andalso
      Util.eqReal(Complex.im(x), ~3.0)
  )
  ;

val _ = UnitTest.test( "plus",
  fn() => 
     let
         val a = Complex.complex(2.0, 3.0);
         val b = Complex.complex(4.3, 2.1);
     in
        Complex.plus a b
     end,
  fn x =>
      Util.eqReal(Complex.re(x), 6.3) andalso
      Util.eqReal(Complex.im(x), 5.1)
  )
  ;

val _ = UnitTest.test( "minus",
  fn() => 
     let
         val a = Complex.complex(2.0, 3.0);
         val b = Complex.complex(4.3, 2.1);
     in
        Complex.minus a b
     end,
  fn x =>
      Util.eqReal(Complex.re(x), ~2.3) andalso
      Util.approxEqReal(Complex.im(x), 0.9, 4)
  )
  ;

val _ = UnitTest.test( "times",
  fn() => 
     let
         val a = Complex.complex(2.0, 3.0);
         val b = Complex.complex(4.3, 2.1);
     in
        (Complex.times a b)
     end,
  fn x =>
      Util.approxEqReal'(Complex.re(x), 2.3, 10) andalso
      Util.approxEqReal'(Complex.im(x), 17.1, 10)
  )
  ;

val _ = UnitTest.test( "divide",
  fn() => 
     let
         val a = Complex.complex(2.0, 3.0);
         val b = Complex.complex(4.3, 2.1);
     in
        (Complex.divide a b)
     end,
  fn x =>
      Util.approxEqReal'(Complex.re(x), 0.6506550218340611353, 10) andalso
      Util.approxEqReal'(Complex.im(x), 0.3799126637554585152, 10)
  )
  ;


val _ = UnitTest.test( "invert",
  fn() => Complex.invert(Complex.complex(2.0, 3.0)),
  fn x =>
     Util.approxEqReal'(Complex.re(x), 0.15384615384615384615, 18) andalso
     Util.approxEqReal'(Complex.im(x), ~0.23076923076923076923, 18)
  )
  ;

val _ = UnitTest.test( "exp",
  fn() => Complex.exp(Complex.complex(2.0, 3.0)),
  fn x =>
     Util.approxEqReal'(Complex.re(x), ~7.3151100949011025174, 18) andalso
     Util.approxEqReal'(Complex.im(x), 1.042743656235904414101, 18)
  )
  ;

val _ = UnitTest.test( "re",
  fn() => Complex.complex(2.0, 3.0),
  fn x =>
     Util.approxEqReal'(Complex.re(x), 2.0, 18)
  )
  ;

val _ = UnitTest.test( "im",
  fn() => Complex.complex(2.0, 3.0),
  fn x =>
     Util.approxEqReal'(Complex.im(x), 3.0, 18)
  )
  ;

val _ = UnitTest.test( "toString",
  fn() => Complex.complex(2.0, 3.0),
  fn x =>
     String.compare(Complex.toString(x), "2 + 3i") = EQUAL
  )
  ;

