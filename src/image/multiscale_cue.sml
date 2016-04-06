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
    image : RealGrayscaleImage.image,
    weights : real list,
    bins : int,
    scale : int, 
    ori : real,
    savgolFilters : (real * real) list) =
  let
    val ( height, width ) = RealGrayscaleImage.dimensions image
    val responseImage = RealGrayscaleImage.zeroImage( height, width )

    val sizes = [ scale div 2, scale, scale*2 ]

    val params = ListPair.zip( sizes, savgolFilters )

    fun calculateResponse( w, ( s, ( savMaj, savMin ) ), a ) =
    let
      val grad = Gradient.orientedGradient( image, bins, s, ori )
      val grad = FilterUtil.savgol( grad, savMaj, savMin, ori+Math.pi/2.0 )
      val _ = RealGrayscaleImage.scale'( grad, w )
      val _ = RealGrayscaleImage.add'( a, grad )
    in
      a
    end
  in
    ListPair.foldl calculateResponse responseImage (weights, params)
  end

  fun multiscaleChannel ( config : channelConfiguration, 
                          image : RealGrayscaleImage.image )
    : RealGrayscaleImage.image =
  let
    val ( height, width ) = RealGrayscaleImage.dimensions image

    val {
      weights = weights,
      savgolFilters = savgolFilters,
      bins = bins,
      scale = scale,
      nori = nori
    } = config

    val ori = List.tabulate 
      ( nori, ( fn i => ( (real i)*Math.pi)/(real nori)) )

    val responses = List.foldl 
      ( fn ( x, a ) => 
        orientedMultiscale( image, weights, bins, scale, x, savgolFilters )::a )
      [] ori
  in
    List.hd responses
    (*ImageUtil.maxRealGrayscale responses *)
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

    val aMultiscale = multiscaleChannel( channelAConfig, aChannelImage )
    val bMultiscale = multiscaleChannel( channelBConfig, bChannelImage )
    val lMultiscale = multiscaleChannel( channelLConfig, lChannelImage )
    val tMultiscale = multiscaleChannel( channelTConfig, textonImage )

    val _ = RealPGM.write(ImageUtil.normalizeReal'' aMultiscale, "output/aMultiscale.pgm" )
    val _ = RealPGM.write(ImageUtil.normalizeReal'' bMultiscale, "output/bMultiscale.pgm" )
    val _ = RealPGM.write(ImageUtil.normalizeReal'' lMultiscale, "output/lMultiscale.pgm" )
    val _ = RealPGM.write(ImageUtil.normalizeReal'' tMultiscale, "output/tMultiscale.pgm" )

    val all = RealGrayscaleImage.add( aMultiscale, bMultiscale )
    val _ = RealGrayscaleImage.add'( all, lMultiscale )
    val _ = RealGrayscaleImage.add'( all, tMultiscale )
  in
    ImageUtil.normalizeReal'' all
  end

end
