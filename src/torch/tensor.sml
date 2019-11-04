(*
* file: tensor.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains the tensor signature and the tensor type functor used
* to create new tensor types.
*)

signature TENSOR =
sig

  type dtype

  val tensor : int list * dtype -> tensor
  val fromList1d : dtype list -> tensor
  val fromList2d : dtype list list -> tensor
  val fromList3d : dtype list list list -> tensor

  val dim : tensor -> int list

  val grad : tensor -> tensor
  val backward : tensor -> tensor
  val cuda : tensor -> tensor

end (* signature TENSOR *)

signature TENSOR_SPEC =
sig
  type dtype
end (* signature TENSOR_SPEC *)

functor TensorFun( Spec : TENSOR_SPEC ) : TENSOR =
struct
  open Spec



end (* functor TensorFun *)
