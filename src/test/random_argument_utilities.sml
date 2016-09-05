(*
* filename: randomArgumentUtilities.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains helper functions for generating random arguments.
*)


structure RandomArgumentUtilities =
struct
  val rand = Random.rand(746773, 882377)

  fun randomInteger( low : int, high : int ) =
    Random.randRange ( low, high ) rand

  fun randomDecimal(low : real, high : real) =
  let 
    val num = Random.randReal rand
  in
    BasicTransformations.rangeToRange ((0.0,1.0), (low, high)) num
  end

  fun randomBSRImage() : string =
  let
    val path = "BSDS_PNM/images" 
    (* Todo: make path a configuration constant somehow *)
    val numImages = 200
    val curImg = randomInteger(0, numImages)
    val dirStream = OS.FileSys.openDir path

    fun findCurrentImgFile(count : int, dirStream : OS.FileSys.dirstream) =
    let
      val img = OS.FileSys.readDir dirStream
    in
      if count = curImg then img
      else findCurrentImgFile(count + 1, dirStream)
    end

    val img = Option.valOf( findCurrentImgFile(0, dirStream) )
    
    val _ = OS.FileSys.closeDir dirStream
  in
    path ^ "/" ^ img
  end
end

