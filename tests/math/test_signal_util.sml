(* 
* file: test_signalUril.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the signal utilities structure
*)


val _ = print"\n\n********** signal utilities tests **********\n"

val _ = UnitTest.test( "dft",
  fn() =>
    let
        val input = Array.fromList([1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
    in
       SignalUtil.dft( Array.vector input )
    end,
  fn x =>
    let
       val correct = [
              Complex.complex(21.0, 0.0),
              Complex.complex(~3.0, 5.1962),
              Complex.complex(~3.0, 1.7321),
              Complex.complex(~3.0, 0.0),
              Complex.complex(~3.0, ~1.7321),
              Complex.complex(~3.0, ~5.1962)
          ];
       val result = List.tabulate(
               Vector.length(x), fn i => Vector.sub(x, i))
    in
       ListPair.allEq (fn (f1,f2) => 
           Util.approxEqReal' (Complex.re(f1), Complex.re(f2), 4) andalso
           Util.approxEqReal' (Complex.im(f1), Complex.im(f2), 4)
           ) (correct, result)
    end
  )
  ;

val _ = UnitTest.test( "idft",
  fn() =>
    let
        val input = Array.fromList([1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
        val transformed = SignalUtil.dft( Array.vector input )
    in
       SignalUtil.idft(transformed)
    end,
  fn x =>
    let
       val correct = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
       val result = List.tabulate(
               Vector.length(x), fn i => Vector.sub(x, i))
    in
       ListPair.allEq 
          (fn (f1, f2) => Util.approxEqReal' (f1, Complex.re(f2), 14) andalso
                          Util.approxEqReal' (0.0, Complex.im(f2), 14))
          (correct, result)
    end
  )


val _ = UnitTest.test( "hilbert",
  fn() =>
    let
        val input = Array.fromList([1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
    in
       SignalUtil.hilbert( Array.vector input )
    end,
  fn x =>
    let
       
       val correct = [
              2.309401076758503,
              ~1.154700538379252,
              ~1.154700538379252,
              ~1.154700538379252,
              ~1.154700538379252,
              2.309401076758503
          ];

       val result = List.tabulate(
               Vector.length(x), fn i => Vector.sub(x, i))

    in
       ListPair.allEq 
          (fn (f1, f2) => Util.approxEqReal' (f1, f2, 3))
          (correct, result)
    end
  )


