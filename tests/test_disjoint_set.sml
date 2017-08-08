(*
* file: test_disjoint_set.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the DisjointSet structure. 
*)


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="DisjointSet", what="init",
    genInput= fn() => [ ( 5, 1.0 ) ] ,
    f= fn[ i1 ] => [ DisjointSet.init i1 ] ,
    evaluate= 
      fn[ o1 ] => [ 
        #1( DisjointSet.find( o1, 0 ) )=0 andalso
        #1( DisjointSet.find( o1, 1 ) )=1 andalso
        #1( DisjointSet.find( o1, 2 ) )=2 andalso
        #1( DisjointSet.find( o1, 3 ) )=3 andalso
        #1( DisjointSet.find( o1, 4 ) )=4 ] ,
    inputToString= fn( n, _ ) => Int.toString n }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="DisjointSet", what="union",
    genInput= 
      fn() =>
      let
        val ds = DisjointSet.init( 5, 1 ) 
      in
        [ ( ds, 1, 2 ), ( ds, 3, 4 ), ( ds, 1, 0 ) ]
      end ,
    f= 
      fn[ i1, i2, i3 ] => 
      let
        val _ = DisjointSet.union i1
        val _ = DisjointSet.union i2
        val _ = DisjointSet.union i3
      in
        [ #1 i1 ]
      end ,
    evaluate= 
      fn[ o1 ] =>
      let
        val ( p0, r0, s0, x0 ) = DisjointSet.find( o1, 0 )
        val ( p1, r1, s1, x1 ) = DisjointSet.find( o1, 1 )
        val ( p2, r2, s2, x2 ) = DisjointSet.find( o1, 2 )
        val ( p3, r3, s3, x3 ) = DisjointSet.find( o1, 3 )
        val ( p4, r4, s4, x4 ) = DisjointSet.find( o1, 4 )
      in 
        [ p0=p1 andalso p1=p2 andalso s0=3 andalso s1=3 andalso s2=3 ]
      end , 
    inputToString= fn( _, i1, i2 ) => Int.toString i1 ^ " " ^ Int.toString i2  }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="DisjointSet", what="find",
    genInput= 
      fn() =>
      let
        val ds = DisjointSet.init( 5, 1 ) 

        val _ = DisjointSet.union( ds, 0, 1 )
        val _ = DisjointSet.union( ds, 0, 4 )
        val _ = DisjointSet.union( ds, 2, 3 )
      in
        [ ( ds, 1 ), ( ds, 0 ), ( ds, 3 ) ]
      end ,
    f= 
      fn[ i1, i2, i3 ] => 
        [ DisjointSet.find i1, DisjointSet.find i2, DisjointSet.find i3 ] ,
    evaluate=
      fn[ ( p1, r1, s1, _ ), ( p2, r2, s2, _ ), ( p3, r3, s3, _ ) ] => 
        [ p1=0 andalso r1=1 andalso s1=3,
          p2=0 andalso r2=1 andalso s2=3,
          p3=2 andalso r3=1 andalso s3=2 ] ,
    inputToString= 
      fn( _, i ) => Int.toString i }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="DisjointSet", what="update",
    genInput= 
      fn() =>
      let
        val ds = DisjointSet.init( 5, 1 ) 

        val _ = DisjointSet.union( ds, 0, 1 )
      in
        [ ( ds, 1, fn _ => 2 ), ( ds, 2, fn _ => 0 ), ( ds, 0, fn _ => 10 ) ]
      end ,
    f= 
      fn[ i1, i2, i3 ] => 
      let
        val ds = #1 i1

        val _ = DisjointSet.update i1
        val _ = DisjointSet.update i2
        val _ = DisjointSet.update i3
      in
        [ ds ]
      end ,
    evaluate=
      fn[ ds ] => 
        [ #4( DisjointSet.find( ds, 1 ) )=10,
          #4( DisjointSet.find( ds, 2 ) )=0,
          #4( DisjointSet.find( ds, 3 ) )=1 ] ,
    inputToString= 
      fn( _, i1, _ ) => Int.toString i1 }
