(* 
* file: test_complex.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the complex structure
*)


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="negative",
    genInput= fn() => [ ( 2.0, 3.0 ) ] ,
    f= fn[ i1 ] => [ Complex.negative( Complex.complex i1 ) ] ,
    evaluate= fn[ o1 ] => [ 
      Real.==( Complex.re o1, ~2.0 ) andalso  
      Real.==( Complex.im o1, ~3.0) ] ,
    inputToString = 
      fn( x, y ) => 
        "( " ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ 
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="plus",
    genInput= fn() => [ ( ( 2.0, 3.0 ), ( 4.3, 2.1 ) ) ] ,
    f= 
      fn[ i1 ] => 
      let
         val a = Complex.complex ( #1 i1 )
         val b = Complex.complex ( #2 i1 )
      in
        [ Complex.plus a b ]
      end,
    evaluate= fn[ o1 ] => [
      Real.==(Complex.re o1, 6.3) andalso
      Real.==(Complex.im o1, 5.1) ] ,
    inputToString = 
      fn( ( x, y ), ( z, w ) ) => 
        "( (" ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ " ), ( " ^ 
        Real.toString z ^ ", " ^ 
        Real.toString w ^ 
        " ) )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="minus",
    genInput= fn() => [ ( ( 2.0, 3.0 ), ( 4.3, 2.1 ) ) ] ,
    f= 
      fn[ i1 ] =>
      let
         val a = Complex.complex ( #1 i1 )
         val b = Complex.complex ( #2 i1 )
      in
        [ Complex.minus a b ]
      end ,
    evaluate= 
      fn[ o1 ] => [
        Real.==( Complex.re o1 , ~2.3 ) andalso
        Util.approxEqReal( Complex.im o1 , 0.9, 4 ) ] ,
    inputToString = 
      fn( ( x, y ), ( z, w ) ) => 
        "( (" ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ " ), ( " ^ 
        Real.toString z ^ ", " ^ 
        Real.toString w ^ 
        " ) )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="times",
    genInput= fn() => [ ( ( 2.0, 3.0 ), ( 4.3, 2.1 ) ) ] ,
    f=
      fn[ i1 ] => 
      let
        val a = Complex.complex ( #1 i1 )
        val b = Complex.complex ( #2 i1 )
      in
        [ Complex.times a b ]
      end ,
    evaluate=
      fn[ o1 ] => [
        Util.approxEqReal'( Complex.re o1, 2.3, 10 ) andalso
        Util.approxEqReal'( Complex.im o1, 17.1, 10 ) ] ,
    inputToString = 
      fn( ( x, y ), ( z, w ) ) => 
        "( (" ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ " ), ( " ^ 
        Real.toString z ^ ", " ^ 
        Real.toString w ^ 
        " ) )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="divide",
    genInput= fn() => [ ( ( 2.0, 3.0 ), ( 4.3, 2.1 ) ) ] ,
    f= 
      fn[ i1 ] =>
      let
        val a = Complex.complex ( #1 i1 )
        val b = Complex.complex ( #2 i1 )
      in
        [ Complex.divide a b ]
      end ,
    evaluate=
      fn[ o1 ] => [
        Util.approxEqReal'( Complex.re o1, 0.6506550218340611353, 10 ) andalso
        Util.approxEqReal'( Complex.im o1, 0.3799126637554585152, 10 ) ] ,
    inputToString = 
      fn( ( x, y ), ( z, w ) ) => 
        "( (" ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ " ), ( " ^ 
        Real.toString z ^ ", " ^ 
        Real.toString w ^ 
        " ) )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="invert",
    genInput= fn() => [ ( 2.0, 3.0 ) ] ,
    f= fn[ i1 ] => [ Complex.invert( Complex.complex i1 ) ] ,
    evaluate=
      fn[ o1 ] => [
        Util.approxEqReal'( Complex.re o1, 0.15384615384615384615, 18 ) andalso
        Util.approxEqReal'( Complex.im o1, ~0.23076923076923076923, 18 ) ] ,
    inputToString = 
      fn( x, y ) => 
        "( " ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ 
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="exp",
    genInput= fn() => [ ( 2.0, 3.0 ) ] ,
    f= fn[ i1 ] => [ Complex.exp( Complex.complex i1 ) ] ,
    evaluate=
      fn[ o1 ] => [
        Util.approxEqReal'( Complex.re o1, ~7.3151100949011025174, 18 ) andalso
        Util.approxEqReal'( Complex.im o1, 1.042743656235904414101, 18 ) ] ,
    inputToString = 
      fn( x, y ) => 
        "( " ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ 
        " )" }

val _ =   
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="re",
    genInput= fn() => [ (2.0, 3.0) ] ,
    f= fn[ i1 ] => [ Complex.complex i1 ] ,
    evaluate= fn[ o1 ] => [ Util.approxEqReal'( Complex.re o1, 2.0, 18 ) ] ,
    inputToString = 
      fn( x, y ) => 
        "( " ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ 
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="im",
    genInput= fn() => [ (2.0, 3.0) ] ,
    f= fn[ i1 ] => [ Complex.complex i1 ] ,
    evaluate= fn[ o1 ] => [ Util.approxEqReal'( Complex.im o1, 3.0, 18 ) ] ,
    inputToString = 
      fn( x, y ) => 
        "( " ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ 
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Complex", what="toString",
    genInput= fn() => [ ( 2.0, 3.0 ) ] ,
    f= fn[ i1 ] => [ Complex.toString( Complex.complex i1 ) ] ,
    evaluate= fn[ o1 ] => [ String.compare( o1, "2 + 3i") = EQUAL ] ,
    inputToString = 
      fn( x, y ) => 
        "( " ^ 
        Real.toString x ^ ", " ^ 
        Real.toString y ^ 
        " )" }
