# Kernel overlap

Computes the kernel overlap as evaluation metric between a predicted
kernel and the ground truth kernel

## Usage

``` r
window_overlap(param_prediction, param_reference)

overlap(
  param_prediction,
  param_reference,
  mix_prediction = NULL,
  mix_reference = NULL
)
```

## Arguments

- param_prediction:

  predicted window parameter matrix (2 columns)

- param_reference:

  ground truth window parameter matrix (2 columns)

- mix_prediction:

  vector of predicted regression parameters

- mix_reference:

  vector of ground truth regression parameters

## Value

a vector of overlap values for each ground truth window

a scalar indicating the overlap between predicted and ground truth
kernel. Value between 0 (minimum overlap) and 1 (complete overlap).

## Functions

- `window_overlap()`: Computes the kernel overlap between each predicted
  window and each ground truth window separately
