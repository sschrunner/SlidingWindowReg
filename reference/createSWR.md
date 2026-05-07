# Create `SWR` model object

Constructor to create an `SWR` model object manually based on model
parameters

## Usage

``` r
createSWR(list = NULL, param = NULL, mix = NULL, ...)
```

## Arguments

- list:

  a list object to be converted to an `SWR` model object

- param:

  a matrix with 2 columns, see details

- mix:

  a vector of mixing parameters (beta), see details

- ...:

  further variables to be appended to the `SWR` model object

## Details

An `SWR` model object is an S3-class object, which contains at least two
variables:

- a matrix `param` with 2 columns representing one window per row. The
  first column contains location parameters delta (\>=0), the second
  column contains the standard deviation sigma (\>0),

- a vector `mix` of regression parameters, representing one window each.
  Thus, the number of windows is given by the number of rows in `param`,
  as well as by the number of elements in `mix`. Additional variables
  can be appended to the `SWR` model object by using the `...` argument
