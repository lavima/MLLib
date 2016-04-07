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
      scale : int,
      nori : int
    }

  type textonConfiguration =
    {
      nori : int,
      sigma : real,
      nTextons : int,
      maxIterations : int
    }

  fun orientedMultiscale(
    height : int, 
    width : int,
    image : 'a,
    gradFun : 'a * int * int * real * (real * real) ->RealGrayscaleImage.image,
    weights : real list,
    bins : int,
    scale : int, 
    ori : real,
    savgolFilters : (real * real) list) =
  let
    val responseImage = RealGrayscaleImage.zeroImage( height, width )

    val sizes = [ scale div 2, scale, scale*2 ]

    val params = ListPair.zip( sizes, savgolFilters )

    fun calculateResponse( w, ( s, savgol ), a ) =
    let
      val grad = gradFun( image, bins, s, ori, savgol )
      val gradScaled = RealGrayscaleImage.scale( grad, w )
      val newA = RealGrayscaleImage.add( a, grad )
    in
      newA
    end
  in
    ListPair.foldl calculateResponse responseImage (weights, params)
  end

  fun multiscaleChannel ( 
    config : channelConfiguration, 
    height : int, 
    width : int,
    image : 'a,
    gradFun : 'a * int * int * real * (real * real) ->RealGrayscaleImage.image )
    : RealGrayscaleImage.image =
  let

    val {
      weights = weights,
      savgolFilters = savgolFilters,
      scale = scale,
      bins = bins,
      nori = nori
    } = config

    val ori = List.tabulate 
      ( nori, ( fn i => ( (real i)*Math.pi)/(real nori)) )

    val responses = List.foldl 
      ( fn ( x, a ) => 
        let
          val _ = print ("Starting ORI:" ^ ( Real.toString x ))
        in
        orientedMultiscale(
          height,
          width,
          image,
          gradFun, 
          weights, 
          bins, 
          scale, 
          x, 
          savgolFilters )::a
        end )
      [] ori
  in
    ImageUtil.maxRealGrayscale responses
  end

  fun multiscale (
    configuration : { 
      channelL : channelConfiguration,
      channelA : channelConfiguration,
      channelB : channelConfiguration,
      channelT : channelConfiguration,
      texton : textonConfiguration,
      border : int
    } )
    ( image : RealRGBImage.image )
    : RealGrayscaleImage.image =
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
      border = borderConfig
    } = configuration

    val extended = RealRGBImage.border 
                 ( RealRGBImage.MirrorExtension, borderConfig )
                 ( image )

    val (height, width) = RealRGBImage.dimensions extended

    val gray = ImageConvert.realRGBtoGray extended
    val _ = FilterUtil.applyGammaCorrectionRealRGB( extended, 2.5 )
    val cie = ImageConvert.realRGBToCIELab extended
    val _ = ImageUtil.normalizeCIELab' cie
    val aChannelImage = ImageUtil.getAChannel cie
    val bChannelImage = ImageUtil.getBChannel cie
    val lChannelImage = ImageUtil.getLChannel cie
    val textonImage = Texton.generateTextons
      ( gray, 
        textonNoriConfig, 
        textonSigmaConfig, 
        textonNTextonsConfig, 
        textonMaxIterationsConfig )

    val ( gradReal, gradInt ) = 
      ( Gradient.orientedGradientReal, Gradient.orientedGradientInt )

    fun realMultiscaleChan( config, channel ) =
       multiscaleChannel( config, height, width, channel, gradReal )

    fun intMultiscaleChan( config, channel ) =
       multiscaleChannel( config, height, width, channel, gradInt )
    
    val aMultiscale = realMultiscaleChan( channelAConfig, aChannelImage )
    val bMultiscale = realMultiscaleChan( channelBConfig, bChannelImage )
    val lMultiscale = realMultiscaleChan( channelLConfig, lChannelImage )
    val tMultiscale = intMultiscaleChan( channelTConfig, textonImage )

    val _ = RealPGM.write(ImageUtil.normalizeReal'' bMultiscale, "output/bMultiscale.pgm" )
    val _ = RealPGM.write(ImageUtil.normalizeReal'' lMultiscale, "output/lMultiscale.pgm" )
    val _ = RealPGM.write(ImageUtil.normalizeReal'' tMultiscale, "output/tMultiscale.pgm" )

    val all = RealGrayscaleImage.add( aMultiscale, bMultiscale )
    val _ = RealGrayscaleImage.add'( all, lMultiscale )
    val _ = RealGrayscaleImage.add'( all, tMultiscale )
    val allNorm = ImageUtil.normalizeReal'' all
  in
    RealGrayscaleImage.trim borderConfig allNorm
  end

end
