// define constants like M_PI and C keywords for MSVC
#ifdef _MSC_VER
#define _USE_MATH_DEFINES
#include <math.h>
#endif
#include <ATen/ATen.h>
#include <ATen/NativeFunctions.h>
#include <ATen/Dispatch.h>
#include <ATen/CPUApplyUtils.h>
#include <ATen/native/BinaryOps.h>
#include <ATen/native/PointwiseOps.h>
#include <ATen/native/TensorIterator.h>

#define EPSILON 1e-12
#define _USE_MATH_DEFINES



namespace {
  static inline at::Tensor apply_loss_reduction(const at::Tensor& unreduced, int64_t reduction) {
    if (reduction == at::Reduction::Mean) {
      return unreduced.mean();
    } else if (reduction == at::Reduction::Sum) {
      return unreduced.sum();
    }
    return unreduced;
  }
}

namespace at { namespace native {

DEFINE_DISPATCH(smooth_l1_stub);
DEFINE_DISPATCH(smooth_l1_backward_stub);

Tensor cosine_embedding_loss(const Tensor& input1, const Tensor& input2, const Tensor& target, double margin, int64_t reduction) {
  auto prod_sum = (input1 * input2).sum(1);
  auto mag_square1 = (input1 * input1).sum(1) + EPSILON;
  auto mag_square2 = (input2 * input2).sum(1) + EPSILON;
  auto denom = (mag_square1 * mag_square2).sqrt_();
  auto cos = prod_sum / denom;

  auto zeros = at::zeros_like(target);
  auto pos = 1 - cos;
  auto neg = (cos - margin).clamp_min_(0);
  auto output_pos = at::where(target == 1, pos, zeros);
  auto output_neg = at::where(target == -1, neg, zeros);
  auto output = output_pos + output_neg;
  return apply_loss_reduction(output, reduction);
}

Tensor hinge_embedding_loss(const Tensor& self, const Tensor& target, double margin, int64_t reduction) {
  auto zeros = at::zeros_like(self);
  auto margin_clamp = (margin - self).clamp_min_(0);
  auto output_margin = at::where(target != 1, margin_clamp, zeros);
  auto output_self = at::where(target != -1, self, zeros);
  auto output = output_margin + output_self;
  return apply_loss_reduction(output, reduction);
}

Tensor triplet_margin_loss(const Tensor& anchor, const Tensor& positive, const Tensor& negative, double margin,
                           double p, double eps, bool swap, int64_t reduction) {
  auto dist_pos = at::pairwise_distance(anchor, positive, p, eps);
  auto dist_neg = at::pairwise_distance(anchor, negative, p, eps);
  if (swap) {
    auto dist_swap = at::pairwise_distance(positive, negative, p, eps);
    dist_neg = at::min(dist_neg, dist_swap);
  }
  auto output = at::clamp_min(margin + dist_pos - dist_neg, 0);
  return apply_loss_reduction(output, reduction);
}

Tensor margin_ranking_loss(const Tensor& input1, const Tensor& input2, const Tensor& target, double margin, int64_t reduction) {
  auto output =  (-target * (input1 - input2) + margin).clamp_min_(0);
  return apply_loss_reduction(output, reduction);
}

Tensor kl_div(const Tensor& input, const Tensor& target, int64_t reduction) {
  auto zeros = at::zeros_like(target);
  auto output_pos = target * (at::log(target) - input);
  auto output = at::where(target > 0, output_pos, zeros);
  return apply_loss_reduction(output, reduction);
}

Tensor kl_div_backward_cpu(const Tensor& grad, const Tensor& input, const Tensor& target, int64_t reduction) {
  auto grad_input = at::zeros_like(input);
  auto grad_expand = grad.expand_as(input);
  AT_DISPATCH_FLOATING_TYPES(input.scalar_type(), "kl_div_backward_cpu", [&]() {
    at::CPU_tensor_apply3<scalar_t, scalar_t, scalar_t>(
        grad_input,
        target,
        grad_expand,
        [] (scalar_t& grad_input_val, const scalar_t& target_val, const scalar_t& grad_val) {
          if (target_val > 0) {
            grad_input_val = -target_val * grad_val;
          }
        });
  });
  if (reduction == at::Reduction::Mean) {
    return grad_input / input.numel();
  }
  return grad_input;
}

Tensor binary_cross_entropy_with_logits(const Tensor& input, const Tensor& target, const Tensor& weight, const Tensor& pos_weight, int64_t reduction) {
    Tensor loss;
    auto max_val = (-input).clamp_min_(0);
    if (pos_weight.defined()) {
        // pos_weight need to be broadcasted, thus mul(target) is not inplace.
        auto log_weight = (pos_weight - 1).mul(target).add_(1);
        loss = (1 - target).mul_(input).add_(log_weight.mul_(((-max_val).exp_().add_((-input - max_val).exp_())).log_().add_(max_val)));
    } else {
        loss = (1 - target).mul_(input).add_(max_val).add_((-max_val).exp_().add_((-input -max_val).exp_()).log_());
    }

    if (weight.defined()) {
        loss.mul_(weight);
    }

    return apply_loss_reduction(loss, reduction);
}

Tensor binary_cross_entropy_with_logits_backward(const Tensor& grad, const Tensor& input, const Tensor& target, const Tensor& weight, const Tensor& pos_weight, int64_t reduction) {
    Tensor grad_input;
    if (pos_weight.defined()) {
        // pos_weight need to be broadcasted, thus mul(target) is not inplace.
        auto t = pos_weight.mul(target);
        grad_input = t.add(1).sub_(target).mul_(input.sigmoid()).sub_(t).mul_(grad);
    } else {
        grad_input = (input.sigmoid() - target).mul_(grad);
    }

    if (weight.defined()) {
        grad_input.mul_(weight);
    }

    if (reduction == at::Reduction::Mean) {
        return grad_input / input.numel();
    }

    return grad_input;
}

Tensor poisson_nll_loss(const Tensor& input, const Tensor& target, const bool log_input, const bool full, const double eps, const int64_t reduction)
{
    Tensor loss;
    if (log_input) {
        loss = at::exp(input) - target * input;
    } else {
        loss = input - target * at::log(input + eps);
    }
    
    if (full) {
        auto mask1 = (target > 1);
        loss.masked_select(mask1) += (target * at::log(target) - target + 0.5 * at::log(2 * M_PI * target)).masked_select(mask1);
    }

    return apply_loss_reduction(loss, reduction);
}

Tensor smooth_l1_loss(const Tensor& input, const Tensor& target, const int64_t reduction) {
  Tensor loss;
  auto iter = TensorIterator::binary_op(loss, input, target);
  smooth_l1_stub(iter.device_type(), iter);
  return apply_loss_reduction(iter.output(), reduction);
}

Tensor& smooth_l1_loss_out(Tensor& result, const Tensor& input, const Tensor& target, int64_t reduction) {
  if (reduction != Reduction::None) {
    result = at::smooth_l1_loss(input, target, reduction);
  } else {
    auto iter = TensorIterator::binary_op(result, input, target);
    smooth_l1_stub(iter.device_type(), iter);
  }
  return result;
}

Tensor& smooth_l1_loss_backward_out(Tensor& grad_input, const Tensor& grad_output, const Tensor& input, const Tensor& target, int64_t reduction) {
  auto norm = reduction == Reduction::Mean ? 1. / input.numel() : 1.;
  auto iter = at::TensorIterator();
  iter.set_check_mem_overlap(true);
  iter.add_output(grad_input);
  iter.add_input(input);
  iter.add_input(target);
  iter.add_input(grad_output);
  iter.build();
  smooth_l1_backward_stub(iter.device_type(), iter, norm);
  return grad_input;
}

Tensor smooth_l1_loss_backward(const Tensor& grad_output, const Tensor& input, const Tensor& target, int64_t reduction) {
  auto grad_input = at::zeros_like(input);
  return at::smooth_l1_loss_backward_out(grad_input, grad_output, input, target, reduction);
}

}}  // namespace at::native
