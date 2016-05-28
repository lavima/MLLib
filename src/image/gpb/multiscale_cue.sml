(*
* file: multiscale_cue.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for generating the multiscale cue.
* 
*)

structure MultiscaleCue =
struct

  type channelConfiguration = 
    {
      weights : real list,
      savgolFilters : ( real * real ) list,
      bins : int,
      scale : int list,
      nori : int,
      histogramSmoothSigma : real option
    }

  type textonConfiguration =
    {
      nori : int,
      sigma : real list,
      nTextons : int,
      maxIterations : int
    }


  fun multiscaleChannel ( 
    config : channelConfiguration, 
    height : int, 
    width : int,
    image : 'a,
    gradientFunction : 'a * int * int * int * ( real * real ) * real option 
    -> RealGrayscaleImage.image list
    )
    : ( RealGrayscaleImage.image list list * RealGrayscaleImage.image list ) =
  let
    val {
      weights = weights,
      savgolFilters = savgolFilters,
      scale = scale,
      bins = bins,
      nori = nori,
      histogramSmoothSigma = histogramSmoothSigma
    } = config

    val responses = List.foldr 
      ( fn ( ( x : int, savgol ), a ) => 
        let
          val _ = print ("Starting Scale:" ^ ( Int.toString x ) ^ "\n" )
         in
          ( gradientFunction
            ( image, bins, nori, x, savgol, histogramSmoothSigma ))::a
        end )
      ( [] )
      ( ListPair.zip( scale, savgolFilters ) )

      val combined =  ListPair.foldl
        ( fn ( images, weight, a ) =>
          let
            val _ = print( "Weight:" ^ ( Real.toString weight ) )
          in
          ListPair.map ( fn ( image, a ) =>
            RealGrayscaleImage.add( a,
              RealGrayscaleImage.scale( image, weight ) ) )
            ( images, a )
          end )
        ( List.tabulate
          ( nori, fn i => RealGrayscaleImage.zeroImage( height, width ) ) )
        ( responses, weights )
  in
    ( responses, combined )
  end

  fun multiscale (
    configuration : { 
      channelL : channelConfiguration,
      channelA : channelConfiguration,
      channelB : channelConfiguration,
      channelT : channelConfiguration,
      texton : textonConfiguration,
      border : int,
      gradientQuantized 
    : IntGrayscaleImage.image * int * int * int * ( real * real ) * real option 
    -> RealGrayscaleImage.image list,
      gradientReal
    : RealGrayscaleImage.image * int * int * int * ( real * real ) * real option
    -> RealGrayscaleImage.image list
    } )
    ( image : RealRGBImage.image )
    : {
        channelL : RealGrayscaleImage.image list list,
        channelA : RealGrayscaleImage.image list list,
        channelB : RealGrayscaleImage.image list list,
        texton   : RealGrayscaleImage.image list list,
        combined : RealGrayscaleImage.image list
      } =
  let
    val  { 
      channelL = channelLConfig,
      channelA = channelAConfig,
      channelB = channelBConfig,
      channelT = channelTConfig,
      texton = 
        {
          nori = textonNoriConfig,
          sigma = textonSigmaConfig,
          nTextons = textonNTextonsConfig,
          maxIterations = textonMaxIterationsConfig
        },
      border = borderConfig,
      gradientQuantized = _,
      gradientReal = _
    } = configuration

    val extended = RealRGBImage.border 
                 ( RealRGBImage.MirrorExtension, borderConfig )
                 ( image )

    val (height, width) = RealRGBImage.dimensions extended

    val gray = ImageConvert.realRGBtoGray extended
    val _ = FilterUtil.applyGammaCorrectionRealRGB( extended, 2.5 )
    val cie = ImageConvert.realRGBToCIELab extended
    val _ = ImageUtil.normalizeCIELab' cie
    val lChannelImage = ImageUtil.getLChannel cie
    val aChannelImage = ImageUtil.getAChannel cie
    val bChannelImage = ImageUtil.getBChannel cie
    val _ = RealPGM.write(lChannelImage, "lChannel.pgm")

    val textonImage = Texton.generateTextons
      ( gray, 
        textonNoriConfig, 
        textonSigmaConfig, 
        textonNTextonsConfig, 
        textonMaxIterationsConfig )

    val ( gradReal, gradInt ) = 
      ( GradientDisk.gradientReal, GradientDisk.gradientQuantized )

    fun realMultiscaleChan( config, channel ) =
       multiscaleChannel( config, height, width, channel, gradReal )

    fun intMultiscaleChan( config, channel ) =
       multiscaleChannel( config, height, width, channel, gradInt )

    val ( lMult, lComb ) = realMultiscaleChan( channelLConfig, lChannelImage )
    val ( aMult, aComb ) = realMultiscaleChan( channelAConfig, aChannelImage )
    val ( bMult, bComb ) = realMultiscaleChan( channelBConfig, bChannelImage )
    val ( tMult, tComb ) = intMultiscaleChan( channelTConfig, textonImage )

    val trimFun = RealGrayscaleImage.trim borderConfig

    fun trimChannelImage( channel : RealGrayscaleImage.image list list )
      : RealGrayscaleImage.image list list =
      List.map (fn s => List.map trimFun s ) channel

    val combined = List.map
      ( fn ( a, ( b, ( l, t ) ) ) => 
          RealGrayscaleImage.add( a, 
            RealGrayscaleImage.add( b,
              RealGrayscaleImage.add( l, t ) ) ) )
      ( ListPair.zip
        ( aComb, ListPair.zip( bComb, ListPair.zip( lComb, tComb ) ) ) )
  in
    {
      channelL = trimChannelImage lMult,
      channelA = trimChannelImage aMult,
      channelB = trimChannelImage bMult,
      texton   = trimChannelImage tMult,
      combined = List.map trimFun combined
     }
  end

end
