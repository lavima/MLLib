#include <ATen/ATen.h>
#include <ATen/CPUApplyUtils.h>
#include <ATen/Config.h>
#include <ATen/Dispatch.h>
#include <ATen/ExpandUtils.h>
#include <ATen/NativeFunctions.h>
#include <c10/util/Exception.h>
#include <ATen/core/EnableNamedTensor.h>

#include <ATen/Utils.h>
#include <ATen/CPUGenerator.h>
#include <ATen/core/DistributionsHelper.h>
#include <ATen/native/Distributions.h>
#include <ATen/native/DispatchStub.h>
#include <ATen/native/UnaryOps.h>
#include <ATen/NamedTensorUtils.h>

#include <type_traits>
#include <functional>
#include <assert.h>
#include <cpuinfo.h>
#include <float.h>

#include <TH/THMath.h>

namespace {
/*
 * This section is a counterpart to Distributions.cu
 *
 */

// The function `sample_poisson`
// is adapted from Numpy's distributions.c implementation.
// It is MIT licensed, so here is the copyright:

/* Copyright 2005 Robert Kern (robert.kern@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */


int64_t sample_poisson(double lambda, at::CPUGenerator* generator) {
  at::uniform_real_distribution<double> standard_uniform(0.0, 1.0);
  if (lambda >= 10) {
    // transformed rejection method, (Hoermann, 1993)
    int64_t k;
    double U, V, a, b, invalpha, vr, us;

    double slam = std::sqrt(lambda);
    double loglam = std::log(lambda);
    b = 0.931 + 2.53 * slam;
    a = -0.059 + 0.02483 * b;
    invalpha = 1.1239 + 1.1328 / (b - 3.4);
    vr = 0.9277 - 3.6224 / (b - 2);

    while (1) {
      U = standard_uniform(generator) - 0.5;
      V = standard_uniform(generator);
      us = 0.5 - std::fabs(U);
      k = (int64_t)std::floor((2 * a / us + b) * U + lambda + 0.43);
      if ((us >= 0.07) && (V <= vr)) {
        return k;
      }
      if ((k < 0) || ((us < 0.013) && (V > us))) {
        continue;
      }
      if ((std::log(V) + std::log(invalpha) - std::log(a / (us * us) + b)) <=
          (-lambda + k * loglam - std::lgamma((double)k + 1))) {
        return k;
      }
    }
  } else if (lambda == 0) {
    return 0;
  } else {
    int64_t X;
    double prod, U, enlam;

    enlam = std::exp(-lambda);
    X = 0;
    prod = 1.0;
    while (1) {
      U = standard_uniform(generator);
      prod *= U;
      if (prod > enlam) {
        X += 1;
      } else {
        return X;
      }
    }
  }
}

} // namespace

namespace at {
namespace native {

Tensor bernoulli(const Tensor& self, Generator* gen) {
  return at::empty_like(self).bernoulli_(self, gen);
}

Tensor bernoulli(const Tensor& self, double p, Generator* gen) {
  return at::empty_like(self).bernoulli_(p, gen);
}

Tensor& bernoulli_out(Tensor& result, const Tensor& self, Generator* gen) {
  // result.resize_as_(self) requires self to have same dtype as result, so we
  // use resize_ instead.
  // TODO: Fix resize_as_. See pytorch/pytorch#11665.
  result.resize_(self.sizes()).bernoulli_(self, gen);
#ifdef BUILD_NAMEDTENSOR
  namedinference::propagate_names(result, self);
#endif
  return result;
}

Tensor& bernoulli_tensor_cpu_(Tensor& self, const Tensor& p_, Generator* gen) {
#ifdef BUILD_NAMEDTENSOR
  NoNamesGuard guard;
#endif
  AT_DISPATCH_ALL_TYPES_AND(at::ScalarType::Bool, self.scalar_type(), "bernoulli_tensor_cpu_self_", [&] {
    CPUGenerator* generator = get_generator_or_default<CPUGenerator>(gen, detail::getDefaultCPUGenerator());
    // See Note [Acquire lock when using random generators]
    std::lock_guard<std::mutex> lock(generator->mutex_);
    using self_t = scalar_t;
    if (p_.scalar_type() == kDouble) {
      auto p = std::get<0>(expand_inplace(self, p_.to(kCPU)));
      CPU_tensor_apply2<self_t, double>(
        self, p, [generator](self_t& ret_val, double& p_val) {
          at::bernoulli_distribution<double> bernoulli(p_val);
          ret_val = static_cast<self_t>(bernoulli(generator));
        });
    } else {
      AT_DISPATCH_FLOATING_TYPES(p_.scalar_type(), "bernoulli_tensor_cpu_p_", [&] {
        auto p = std::get<0>(expand_inplace(self, p_.to(kCPU)));
        using p_t = scalar_t;
        CPU_tensor_apply2<self_t, p_t>(
          self, p, [generator](self_t& ret_val, p_t& p_val) {
            at::bernoulli_distribution<float> bernoulli(p_val);
            ret_val = static_cast<self_t>(bernoulli(generator));
        });
      });
    }
  });
  return self;
}

DEFINE_DISPATCH(bernoulli_mkl_stub);

Tensor& bernoulli_scalar_cpu_(Tensor& self, double p, Generator* gen) {
  TORCH_CHECK(0 <= p && p <= 1, "bernoulli_ expects p to be in [0, 1], but got p=", p);
#if AT_MKL_ENABLED()
  if (cpuinfo_initialize() && cpuinfo_vendor_intel == cpuinfo_get_processor(0)->core->vendor) {
    bernoulli_mkl_stub(kCPU, self, p, gen);
    return self;
  }
#endif
  AT_DISPATCH_ALL_TYPES_AND(at::ScalarType::Bool, self.scalar_type(), "bernoulli_scalar_cpu_", [&] {
    CPUGenerator* generator = get_generator_or_default<CPUGenerator>(gen, detail::getDefaultCPUGenerator());
    // See Note [Acquire lock when using random generators]
    std::lock_guard<std::mutex> lock(generator->mutex_);
    CPU_tensor_apply1<scalar_t>(
        self, [generator, p](scalar_t& ret_val) {
          at::bernoulli_distribution<double> bernoulli(p);
          ret_val = static_cast<scalar_t>(bernoulli(generator));
        });
  });
  return self;
}


Tensor _standard_gamma_grad_cpu(const Tensor& self, const Tensor& output) {
  Tensor ret = at::empty(self.sizes(), self.options());
  AT_DISPATCH_FLOATING_TYPES(self.scalar_type(), "_standard_gamma_grad_cpu", [&] {
    CPU_tensor_apply3<scalar_t, scalar_t, scalar_t>(ret, self, output,
      [](scalar_t& ret_val, const scalar_t& self_val, const scalar_t &output_val) {
        ret_val = standard_gamma_grad_one<scalar_t, double>(self_val, output_val);
      }
    );
  });
  return ret;
}

Tensor _dirichlet_grad_cpu(const Tensor& x, const Tensor& alpha, const Tensor& total) {
  Tensor ret = at::empty(x.sizes(), x.options());
  AT_DISPATCH_FLOATING_TYPES(x.scalar_type(), "_dirichlet_grad_cpu", [&] {
    CPU_tensor_apply4<scalar_t, scalar_t, scalar_t, scalar_t>(ret, x, alpha, total,
      [](scalar_t& ret_val, const scalar_t& x_val, const scalar_t& alpha_val, const scalar_t& total_val) {
        ret_val = dirichlet_grad_one<scalar_t, double>(x_val, alpha_val, total_val);
      }
    );
  });
  return ret;
}

/*
 * This section is a counterpart to Distributions.cu
 */

Tensor _s_poisson_cpu(const Tensor& lambda, Generator *gen) {
  Tensor ret = at::zeros(lambda.sizes(), lambda.options());
  AT_DISPATCH_FLOATING_TYPES(ret.scalar_type(), "poisson_cpu", [&] {
    CPUGenerator* generator = get_generator_or_default<CPUGenerator>(gen, detail::getDefaultCPUGenerator());
    // See Note [Acquire lock when using random generators]
    std::lock_guard<std::mutex> lock(generator->mutex_);
    CPU_tensor_apply2<scalar_t, scalar_t>(ret, lambda,
      [generator](scalar_t& ret_val, const scalar_t& lambda){
        ret_val = static_cast<scalar_t>(sample_poisson(static_cast<double>(lambda), generator));
      }
    );
    });
  return ret;
}

Tensor _s_gamma_cpu(const Tensor& alpha, Generator *gen) {
  Tensor ret = at::zeros(alpha.sizes(), alpha.options());
  AT_DISPATCH_FLOATING_TYPES(ret.scalar_type(), "gamma_cpu", [&] {
    CPUGenerator* generator = get_generator_or_default<CPUGenerator>(gen, detail::getDefaultCPUGenerator());
    // See Note [Acquire lock when using random generators]
    std::lock_guard<std::mutex> lock(generator->mutex_);
    CPU_tensor_apply2<scalar_t, scalar_t>(ret, alpha,
      [generator](scalar_t& ret_val, const scalar_t& alpha){

        auto uniform_lambda = [generator] () {
          at::uniform_real_distribution<double> standard_uniform(0.0, 1.0);
          return standard_uniform(generator);
        };
        BaseSampler<double, decltype(uniform_lambda)> standard_uniform(uniform_lambda);

        auto normal_lambda = [generator] () {
          at::normal_distribution<double> normal(0.0, 1.0);
          return normal(generator);
        };
        BaseSampler<double, decltype(normal_lambda)> standard_normal(normal_lambda);
        auto sample = sample_gamma<scalar_t, double, decltype(uniform_lambda), decltype(normal_lambda)>(alpha, standard_uniform, standard_normal);
        ret_val = std::max(std::numeric_limits<scalar_t>::min(), (scalar_t) sample);
      }
    );
    });

  return ret;
}

Tensor _s_dirichlet_cpu(const Tensor& alpha, Generator *gen) {
  Tensor ret = at::zeros(alpha.sizes(), alpha.options());
  AT_DISPATCH_FLOATING_TYPES(ret.scalar_type(), "dirichlet", [&] {
    Tensor gamma = at::zeros(alpha.sizes(), alpha.options().dtype(ScalarType::Double));
    CPUGenerator* generator = get_generator_or_default<CPUGenerator>(gen, detail::getDefaultCPUGenerator());
    // See Note [Acquire lock when using random generators]
    std::lock_guard<std::mutex> lock(generator->mutex_);
    /* Generate gamma sample by casting alpha to double to prevent underflow. */
    CPU_tensor_apply2<double, scalar_t>(gamma, alpha,
      [generator](double& ret_val, const scalar_t& alpha){
        auto uniform_lambda = [generator] () {
          at::uniform_real_distribution<double> standard_uniform(0.0, 1.0);
          return standard_uniform(generator);
        };
        BaseSampler<double, decltype(uniform_lambda)> standard_uniform(uniform_lambda);

        auto normal_lambda = [generator] () {
          at::normal_distribution<double> normal(0.0, 1.0);
          return normal(generator);
        };
        BaseSampler<double, decltype(normal_lambda)> standard_normal(normal_lambda);
        auto sample = sample_gamma<double, double, decltype(uniform_lambda), decltype(normal_lambda)>
          (alpha, standard_uniform, standard_normal);
        ret_val = std::max(std::numeric_limits<double>::min(), sample);
      }
    );
    /* Normalize and cast back to scalar_t. */
    Tensor gamma_sum = gamma.sum(-1, true).expand(alpha.sizes());
    CPU_tensor_apply3<scalar_t, double , double>(ret, gamma, gamma_sum,
      [](scalar_t& ret_val, const double& gamma, const double& gamma_sum){
        ret_val = gamma / gamma_sum;
        auto min_val = std::numeric_limits<scalar_t>::min();
        auto max_val = std::nexttoward(static_cast<scalar_t>(1.0f), 0.0f);
        ret_val = std::min(max_val, std::max(min_val, ret_val));
        ret_val = static_cast<scalar_t>(ret_val);
      }
    );
  });
  return ret;
}

/* The largest consecutive integer representable in float32 (2^24) */
constexpr int64_t FLOAT32_MAX_CONSECUTIVE_INT = 1 << (FLT_MANT_DIG);

Tensor& multinomial_out(Tensor& result, const Tensor& self, int64_t n_sample, bool with_replacement, Generator *gen) {
  TORCH_CHECK(result.device() == self.device(), "multinomial arguments must have the same device");
  TORCH_CHECK(self.dim() > 0 && self.dim() <= 2, "prob_dist must be 1 or 2 dim");
  TORCH_CHECK(at::isFloatingType(self.scalar_type()),
      "multinomial only supports floating-point dtypes for input, got: ", self.scalar_type());
  TORCH_CHECK(result.scalar_type() == ScalarType::Long,
      "multinomial expects Long tensor out, got: ", result.scalar_type());
  TORCH_CHECK(n_sample > 0, "cannot sample n_sample <= 0 samples");
  int64_t n_categories = self.size(-1);
  TORCH_CHECK(with_replacement || (n_sample <= n_categories),
      "cannot sample n_sample > prob_dist.size(-1) samples without replacement");
  // Since the index tensor is float, numCategories cannot exceed max
  // float integer precision
  TORCH_CHECK(n_categories <= FLOAT32_MAX_CONSECUTIVE_INT, "number of categories cannot exceed 2^24");
  if (self.dim() > 1) {
    int64_t n_dist = self.size(-2);
    result.resize_({n_dist, n_sample});
  } else {
    result.resize_({n_sample});
  }
  multinomial_stub(result.type().device_type(), result, self, n_sample, with_replacement, gen);
  return result;
}

Tensor multinomial(const Tensor& self, int64_t n_sample, bool with_replacement, Generator *gen) {
  Tensor result = at::empty({0}, self.options().dtype(kLong));
  native::multinomial_out(result, self, n_sample, with_replacement, gen);
  return result;
}

DEFINE_DISPATCH(multinomial_stub);

}} // namespace at::native
