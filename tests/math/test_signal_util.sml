(* 
* file: test_signal_util.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the signal utilities structure
*)


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="SignalUtil", what="dft",
    genInput= 
      fn() => [ 
        Array.vector( Array.fromList[ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] ) ] ,
    f= fn[ i1 ] => [ SignalUtil.dft i1 ] ,
    evaluate=
      fn[ o1 ] =>
      let
        val correct = [
          Complex.complex( 21.0, 0.0 ),
          Complex.complex( ~3.0, 5.1962 ),
          Complex.complex( ~3.0, 1.7321 ),
          Complex.complex( ~3.0, 0.0 ),
          Complex.complex( ~3.0, ~1.7321 ),
          Complex.complex( ~3.0, ~5.1962 ) ]
        val result = 
          List.tabulate( Vector.length o1, fn i => Vector.sub( o1, i ) )
      in
        [ ListPair.allEq 
            ( fn ( f1,f2 ) => 
                Util.approxEqReal' ( Complex.re f1, Complex.re( f2 ), 4 ) 
                andalso
                Util.approxEqReal' ( Complex.im f1, Complex.im f2, 4 ) ) 
            ( correct, result ) ]
      end ,
    inputToString = VectorUtil.toString Real.toString } 

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="SignalUtil", what="idft",
    genInput= 
      fn() => [ 
        Array.vector( Array.fromList[ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] ) ] ,
    f= fn[ i1 ] => [ SignalUtil.idft( SignalUtil.dft i1 ) ] ,
    evaluate= 
      fn[ o1 ] =>
      let
         val correct = [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ];
         val result = 
          List.tabulate( Vector.length o1, fn i => Vector.sub( o1, i ) )
      in
        [ ListPair.allEq 
            ( fn (f1, f2) => 
                Util.approxEqReal' ( f1, Complex.re f2, 14 ) andalso
                Util.approxEqReal' ( 0.0, Complex.im(f2), 14 ) )
            ( correct, result ) ]
      end ,
    inputToString = VectorUtil.toString Real.toString } 

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="SignalUtil", what="hilbert",
    genInput= 
      fn() => [ 
        Array.vector( Array.fromList[ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] ) ] ,
    f= fn[ i1 ] => [ SignalUtil.hilbert i1 ] ,
    evaluate= 
      fn[ o1 ] =>
      let
        val correct = [
                2.309401076758503,
                ~1.154700538379252,
                ~1.154700538379252,
                ~1.154700538379252,
                ~1.154700538379252,
                2.309401076758503 ]

        val result = 
          List.tabulate( Vector.length o1, fn i => Vector.sub( o1, i ) )
      in
        [ ListPair.allEq 
            ( fn( f1, f2 ) => Util.approxEqReal' ( f1, f2, 3 ) )
            ( correct, result ) ]
      end ,
    inputToString = VectorUtil.toString Real.toString } 




