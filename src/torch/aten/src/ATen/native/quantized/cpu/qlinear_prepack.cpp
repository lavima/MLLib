#include <ATen/ATen.h>
#include <ATen/core/op_registration/op_registration.h>
#include <ATen/cpp_custom_type_hack.h>
#include <ATen/native/quantized/cpu/fbgemm_utils.h>
#include <ATen/native/quantized/cpu/init_qnnpack.h>
#include <ATen/native/quantized/cpu/qnnpack_utils.h>
#include <ATen/quantized/Quantizer.h>
#include <algorithm>
#include <vector>

namespace caffe2 {
#ifdef USE_FBGEMM
// Required for cpp_custom_type_hack to work
CAFFE_KNOWN_TYPE(PackedLinearWeight);
#endif // USE_FBGEMM
#ifdef USE_PYTORCH_QNNPACK
// Required for cpp_custom_type_hack to work
CAFFE_KNOWN_TYPE(PackedLinearWeightsQnnp);
#endif // USE_PYTORCH_QNNPACK
} // namespace caffe2

namespace at {
namespace native {
namespace {

class QLinearPackWeightInt8 final : public c10::OperatorKernel {
 public:
#ifdef USE_FBGEMM
  // Calculate the column offsets.
  // Note this includes the sum of the columns as well as the scalar term
  // B_zero_point * K, whereas the row_offsets created by
  // PackAWithQuantRowOffset is only the sum of the A rows.
  void calc_col_offsets_transpose(
      int K,
      int N,
      const int8_t* Bint8,
      int32_t* B_zero_point,
      int32_t* col_offsets,
      c10::QScheme qtype) {
    for (size_t i = 0; i < N; ++i) {
      int32_t sum = 0;
      for (size_t j = 0; j < K; ++j) {
        sum += Bint8[i * K + j];
      }
      if (qtype == kPerTensorAffine) {
        col_offsets[i] = sum - B_zero_point[0] * K;
      } else {
        col_offsets[i] = sum - B_zero_point[i] * K;
      }
    }
  }
  at::Tensor fbgemm_linear_prepack(
      at::Tensor weight,
      c10::optional<Tensor> bias) {
    TORCH_CHECK(
        weight.dim() == 2,
        "The weight tensor for quantized::linear_prepack (fbgemm) should"
        " be 2-dimensional.");

    auto N = weight.size(0);
    auto K = weight.size(1);

    // TODO: contiguous is called for further JIT optimizations.
    auto weight_contig = weight.contiguous();
    const auto qtype = weight.qscheme();
    std::vector<int32_t> weight_zero_points_int32(1, 0);
    if (qtype == kPerTensorAffine) {
      weight_zero_points_int32[0] = weight.q_zero_point();
    } else if (qtype == kPerChannelAffine) {
      weight_zero_points_int32.resize(N, 0);
      for (int i = 0; i < N; ++i) {
        weight_zero_points_int32[i] =
            weight.q_per_channel_zero_points()[i].item<int32_t>();
      }
    }
    std::vector<float> weight_scales_float(1, 0.0);
    if (qtype == kPerTensorAffine) {
      weight_scales_float[0] = weight.q_scale();
    } else if (qtype == kPerChannelAffine) {
      weight_scales_float.resize(N, 0.0);
      for (int i = 0; i < N; ++i) {
        weight_scales_float[i] = weight.q_per_channel_scales()[i].item<float>();
      }
    }

    int8_t* weight_ptr_int8 =
        reinterpret_cast<int8_t*>(weight_contig.data_ptr<c10::qint8>());

    std::vector<int32_t> col_offsets(N);
    calc_col_offsets_transpose(
        /*K=*/K,
        /*N=*/N,
        /*Bint8=*/weight_ptr_int8,
        /*B_zero_point=*/weight_zero_points_int32.data(),
        /*col_offsets=*/col_offsets.data(),
        /*qtype=*/qtype);

    c10::optional<at::Tensor> bias_contig;
    if (bias.has_value()) {
      Tensor bias_vec = bias.value();
      TORCH_CHECK(bias_vec.dim() == 1, "bias should be a vector (1D Tensor)");
      TORCH_CHECK(
          bias_vec.size(0) == N,
          "bias should have N elements: " + std::to_string(N));
      bias_contig = bias->contiguous();
    }
    auto ret_ptr = guts::make_unique<PackedLinearWeight>(PackedLinearWeight{
        guts::make_unique<fbgemm::PackBMatrix<int8_t>>(
            /*trans=*/fbgemm::matrix_op_t::Transpose,
            /*nRow=*/K,
            /*nCol=*/N,
            /*smat=*/weight_ptr_int8,
            /*ld=*/K,
            /*pmat=*/nullptr, // PackBMatrix manages ownership of pmat
            /*groups=*/1),
        bias_contig,
        col_offsets,
        weight_scales_float,
        weight_zero_points_int32,
        qtype});

    // TODO: we will need to replace this with torchscript classes at a later
    // point.
    return cpp_custom_type_hack::create(std::move(ret_ptr), weight.options());
  }
#endif
#ifdef USE_PYTORCH_QNNPACK
  at::Tensor qnnpack_linear_prepack(
      at::Tensor weight,
      c10::optional<Tensor> bias_in) {
    TORCH_CHECK(
        weight.dim() == 2,
        "quantized::linear_prepack (qnnpack): Weight tensor rank should be == 2");
    TORCH_CHECK(
        weight.qscheme() == kPerTensorAffine,
        "quantized::linear_prepack (qnnpack) only supports Per Tensor Quantization Scheme")

    int64_t rows_w = weight.size(0);
    Tensor bias_fp32;
    if (bias_in.has_value()) {
      bias_fp32 = bias_in.value();
    } else {
      bias_fp32 = at::zeros(rows_w, weight.options().dtype(at::kFloat));
    }
    TORCH_CHECK(
        !bias_fp32.defined() || (bias_fp32.ndimension() == 1 && bias_fp32.size(0) == rows_w),
        "quantized::linear_prepack (qnnpack): Given weight of size ",
        weight.sizes(),
        ", expected bias to be 1-dimensional with ",
        rows_w,
        " elements",
        ", but got bias of size ",
        bias_fp32.sizes(),
        " instead");

    Tensor weight_contig = weight.contiguous();
    auto weight_zp = weight.q_zero_point() + 128;

    int8_t* inp_data = (int8_t*)weight_contig.data_ptr<c10::qint8>();
    Tensor qnnp_weight = at::_empty_affine_quantized(
        weight_contig.sizes(),
        at::device(kCPU).dtype(kQUInt8),
        weight.q_scale(),
        weight_zp);
    auto* qnnp_w_data = qnnp_weight.data_ptr<c10::quint8>();
    auto wt_numel = weight_contig.numel();
    for (int i = 0; i < wt_numel; ++i) {
      qnnp_w_data[i] = static_cast<c10::quint8>(inp_data[i] + 128);
    }
    initQNNPACK();

    // We set the pre-packed linear weights to nullptr below as we call pre-pack
    // during the first invocation of operator run. Refer to qlinear.cpp for more
    // details. TODO Update to actually call pre-pack here once bias is removed
    // from pre-packing step.
    auto wt_ptr = guts::make_unique<PackedLinearWeightsQnnp>(
        PackedLinearWeightsQnnp{nullptr,
                                weight_contig, /* int8_t weight */
                                bias_fp32.contiguous(), /* fp32 bias */
                                c10::nullopt, /* input_scale */
                                weight.q_scale(),
                                weight_zp});
    return cpp_custom_type_hack::create(std::move(wt_ptr), weight.options());
  }
#endif
  at::Tensor operator()(at::Tensor weight, c10::optional<Tensor> bias) {
    auto& ctx = at::globalContext();

#ifdef USE_FBGEMM
    if (ctx.qEngine() == at::QEngine::FBGEMM) {
      return fbgemm_linear_prepack(weight, bias);
    }
#endif
#ifdef USE_PYTORCH_QNNPACK
    if (ctx.qEngine() == at::QEngine::QNNPACK) {
      return qnnpack_linear_prepack(weight, bias);
    }
#endif
    TORCH_CHECK(
        false,
        "Didn't find engine for operation quantized::linear_prepack ",
        toString(ctx.qEngine()));
  }
};

static auto registry = c10::RegisterOperators().op(
    "quantized::linear_prepack(Tensor W, Tensor? B=None) -> Tensor W_prepack",
    c10::RegisterOperators::options().kernel<QLinearPackWeightInt8>(
        TensorTypeId::QuantizedCPUTensorId));
} // namespace
} // namespace native
} // namespace at
