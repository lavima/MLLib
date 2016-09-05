(*
* file: test_pcnn.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file validates the PCNN functionality.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="PCNN", what="Testing PCNN.fastLinkingIterate",
    genInput=
      fn() =>
      let
        val im = Option.valOf( RealPGM.read "12003.pgm" )
        val ( height, width ) = RealGrayscaleImage.dimensions im

        fun gauss( D : real ) : real =
          8.0 * ( ( 1.0 / ( 2.50662 ) )*Math.exp( ~1.0*( D*D )/( 2.0 ) ) )

      in
        [ ( im,
            ( height, 
              width,
              ~0.634852964216, 
               0.469721425587, 
              ~0.376009763869, 
               0.61915438471, 
               1.11356209154, 
               0.508864990111, 
               2.67338210443,
               gauss,
               gauss ) ) ]
      end ,
    f=
      fn[ i1 ] =>
      let
        val img = #1 i1 
        val pcnn = PCNN.create ( #2 i1 )

        val _ = PCNN.fastLinkingIterate(pcnn, 1.5, 16, img)
        val _ = BooleanPBM.write(#y pcnn, "output/pcnn_1.pbm" )
        val _ = PCNN.fastLinkingIterate(pcnn, 1.5, 16, img)
        val _ = BooleanPBM.write(#y pcnn, "output/pcnn_2.pbm" )
        val _ = PCNN.fastLinkingIterate(pcnn, 1.5, 16, img)
        val _ = BooleanPBM.write(#y pcnn, "output/pcnn_3.pbm" )

        val y = #y pcnn
      in
        [ y ]
      end ,
    evaluate=
      fn[ o1 ] =>
      let
        val _ = BooleanPBM.write( o1, "output/pcnn.pbm" )
      in
        [ true ]
      end ,
    inputToString=
      fn( i, p ) => RealGrayscaleImage.toString i }
