# Predict target variable

Predicts the output time series given an input time series using a
trained `SWR` model object.

## Usage

``` r
# S3 method for class 'SWR'
predict(object, newdata, ...)
```

## Arguments

- object:

  an `SWR` model object created using
  [trainSWR](https://sschrunner.github.io/SlidingWindowReg/reference/trainSWR.md)

- newdata:

  a vector or ts object of new model inputs to predict

- ...:

  currently unused
