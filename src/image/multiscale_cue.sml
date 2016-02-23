(*
* file: multiscale_cue.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for generating the multiscale cue.
* 
*)


structure MultiscaleCue =
struct
  fun orientedMultiscale(
    image : GrayscaleImageReal.image,
    weights : real list,
    bins : int,
    scale : int, 
    ori : real,
    savgolFilters : (real * real) list) =
  let
    val { width = width, height = height, ... } = image
    val responseImage = GrayscaleImageReal.zeroImage(width, height)

    val sizes = [scale div 2, scale, scale * 2]

    val params = ListPair.zip(sizes, savgolFilters)

    fun calculateResponse(w, (s, (savMaj, savMin)), a) =
    let
      val grad = Gradient.orientedGradient(image, bins, s, ori)
      val grad = FilterUtil.savgol(grad, savMaj, savMin, ori + Math.pi / 2.0)
      val _ = GrayscaleImageReal.modify (fn p => p * w) grad
      val _ = GrayscaleImageReal.add' (a, grad)
    in
      a
    end
  in
    ListPair.foldl calculateResponse responseImage (weights, params)
  end

  fun multiscale( 
    channelA : GrayscaleImageReal.image,
    textonBase : GrayscaleImageReal.image,
    weights : real list,
    bins : int,
    scale : int,
    nori : int) : GrayscaleImageReal.image =
  let
    val { width = width, height = height, ... } = channelA

    val savgol_A = [(3.0, 3.0 / 4.0), (5.0, 5.0 / 4.0), (10.0, 10.0 / 4.0)]
    val savgol_T = [(5.0, 5.0 / 4.0), (10.0, 10.0 / 4.0), (20.0, 20.0 / 4.0)]

    val ori = List.tabulate (nori, (fn i => ((real i) * Math.pi) / (real nori)))

    val responses = List.foldl 
       (fn (x, a) => orientedMultiscale
                 (channelA, weights, bins, scale, x, savgol_A)::a) [] ori

    val textonImage = Texton.generateTextons(textonBase, 8, 2.0, 32, 500)  

    val responses = responses @ List.foldl 
       (fn (x, a) => orientedMultiscale
                 (textonImage, weights, bins, scale, x, savgol_T)::a) [] ori
    
    fun maxResponse(x : int, y : int) : real =
      List.foldl (fn (resp, a) => 
              Real.max(GrayscaleImageReal.sub(resp, x, y), a)) 0.0 responses

    val responseImage = GrayscaleImageReal.tabulatexy 
      (width, height, maxResponse)
  in
     responseImage
  end

  
  
end
