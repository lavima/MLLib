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

  fun labelComponents (image : BooleanImage.image) : GrayscaleImageInt.image =
  let
    val { width = width, height = height, ... } = image
    val output = GrayscaleImageInt.zeroImage (width, height)

    fun labelComponent(labelNo : int, x : int, y : int) : int = 
      if
        x < 0 orelse 
        y < 0 orelse 
        x > width - 1 orelse
        y > height - 1 orelse 
        GrayscaleImageInt.sub(output, x, y) <> 0 orelse
        BooleanImage.sub(image, x, y) = false then labelNo
      else
      let
        val _ = GrayscaleImageInt.update(output, x, y, labelNo)
        val _ = labelComponent(labelNo, x - 1, y)
        val _ = labelComponent(labelNo, x + 1, y)
        val _ = labelComponent(labelNo, x, y - 1)
        val _ = labelComponent(labelNo, x, y + 1)
      in
        labelNo
      end

    fun labelNewComponent(labelNo : int, x : int, y : int) : int =
      if BooleanImage.sub(image, x, y) <> false andalso 
         GrayscaleImageInt.sub(output, x, y) = 0 then
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
