(*
* file: connected-components.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for labeling components
* 
*)


structure ConnectedComponents =
struct

  (*
   * Uniquely label the components in a boolean image using
   * a naive appraoch.
   *)

  fun labelComponents (image : BooleanImage.image) : IntGrayscaleImage.image =
  let
    val ( height, width ) = BooleanImage.dimensions image
    val output = IntGrayscaleImage.zeroImage( height, width )

    fun labelComponent(labelNo : int, x : int, y : int) : int = 
      if
        x < 0 orelse 
        y < 0 orelse 
        x > width - 1 orelse
        y > height - 1 orelse 
        IntGrayscaleImage.sub(output, y, x) <> 0 orelse
        BooleanImage.sub(image, y, x) = false then labelNo
      else
      let
        val _ = IntGrayscaleImage.update(output, y, x, labelNo)
        val _ = labelComponent(labelNo, x - 1, y)
        val _ = labelComponent(labelNo, x + 1, y)
        val _ = labelComponent(labelNo, x, y - 1)
        val _ = labelComponent(labelNo, x, y + 1)
      in
        labelNo
      end

    fun labelNewComponent(labelNo : int, x : int, y : int) : int =
      if BooleanImage.sub(image, y, x) <> false andalso 
         IntGrayscaleImage.sub(output, y, x) = 0 then
        labelComponent(labelNo + 1, x, y)
      else labelNo

    fun labelComponents(labelNo, x, y) =
      if y > height - 1 then ()
      else if x > width - 1 then 
        labelComponents(labelNo, 0, y + 1)
      else 
        labelComponents(labelNewComponent(labelNo, x, y), x + 1, y)

    val _ = labelComponents(0, 0, 0)
  in
    output
  end
end
